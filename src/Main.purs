module Main where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Control.Monad.ST (run, while) as ST
import Control.Monad.ST.Ref (modify, new, read, write) as ST
import Data.Array ((!!), (..), (:))
import Data.Array as Array
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Array.ST as STArray
import Data.Array.ST.Partial as STArray.Partial
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe', isJust, maybe')
import Data.Newtype (unwrap)
import Data.Ord (abs)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex (match) as Regex
import Data.String.Regex.Flags (noFlags) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Effect (Effect, untilE)
import Effect.Aff (launchAff_)
import Effect.Class.Console (logShow)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Options.Applicative (Parser, ParserInfo, ReadM, argument, command, eitherReader, execParser, fullDesc, header, help, helper, hsubparser, info, int, long, metavar, option, progDesc, short, showDefault, str, value, (<**>))
import Options.Applicative.Help (displayS, extractChunk, flatten, renderCompact)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Record as Record
import Type.Row (type (+))

progName :: String
progName = "sheytanabi2"

version :: String
version = "0.0.1"

data Target
  = Print
  | File FilePath

derive instance Eq Target
derive instance Ord Target
derive instance Generic Target _
instance Show Target where
  show x = genericShow x

data Command
  = Noop
  | Count
  | Write Target

derive instance Eq Command
derive instance Ord Command
derive instance Generic Command _
instance Show Command where
  show x = genericShow x

type GlobalConfigR r =
  ( colsNum :: Int
  , maxCellValue :: Int
  | r
  )

type LimitsConfigR r =
  ( consecutiveGroupNumLimit :: Int
  , maxConsecutiveLimit :: Int
  , colLimits :: Array { upper :: Int, lower :: Int }
  | r
  )

type GlobalConfig = { | GlobalConfigR () }
type LimitsConfig = { | LimitsConfigR () }

type Config = { command :: Command | GlobalConfigR + LimitsConfigR + () }

type State = Array Int

defaultColLimits :: forall r. { | GlobalConfigR r } -> Array { upper :: Int, lower :: Int }
defaultColLimits config =
  let
    colsNum = config.colsNum
    maxValue = config.maxCellValue
  in
    Array.foldr (\el res -> { lower: colsNum - el, upper: maxValue - el } : res) [] (0 .. (colsNum - 1))

defaultConfig :: Config
defaultConfig =
  let
    global = { colsNum: 6, maxCellValue: 60 }
    limits' = { colLimits: [], consecutiveGroupNumLimit: 2, maxConsecutiveLimit: 2 }
    limits = limits' { colLimits = defaultColLimits global }
    config = Record.merge global limits # Record.merge { command: Noop }
  in
    config

-- data ProgressStatus = NoMore | IncNext | CheckNext

main :: Effect Unit
main = do
  let parseInfo = optInfo defaultConfig
  let hdr = parseInfo # unwrap # (_.infoHeader) <#> flatten # extractChunk # renderCompact # displayS
  config <- execParser parseInfo
  Console.log hdr
  logShow defaultConfig
  logShow config
  case colLimitsOK config of
    Left err -> Console.error $ "Kolon limitlerinde hata oluştu: \n\t" <> String.joinWith "\n\t" err
    Right _ -> findResult config $ map (_.lower) config.colLimits

findResult :: Config -> State -> Effect Unit
findResult config initial = do
  cnt <- go initial
  case config.command of
    Count -> Console.log $ "Seçilen kombinasyon sayısı: " <> show cnt
    _ -> pure unit
  where
  go cols = do
    colsRef <- Ref.new cols
    countRef <- Ref.new 0
    untilE do
      cs <- Ref.read colsRef
      when (valid config cs) do
        let textCols = String.joinWith ";" $ map show $ Array.reverse cols
        case config.command of
          Noop -> Ref.write (config.colLimits <#> (_.upper)) colsRef
          Count -> Ref.modify_ inc countRef
          Write Print -> do
            Console.log textCols
            Ref.modify_ inc countRef
          Write (File path) -> do
            launchAff_ $ FS.appendTextFile UTF8 path textCols
            Ref.modify_ inc countRef
      Ref.modify (progress config) colsRef <#> Array.null
    Ref.read countRef

-- go cnt = case _ of
--   [] -> pure cnt
--   cols
--     | valid config cols -> do
--         let textCols = String.joinWith ";" $ map show $ Array.reverse cols
--         case config.command of
--           Noop -> go cnt []
--           Count -> go (cnt + 1) (progress config cols)
--           Write Print -> do
--             Console.log textCols
--             go (cnt + 1) (progress config cols)
--           Write (File path) -> do
--             FS.appendTextFile UTF8 path textCols
--             go (cnt + 1) (progress config cols)
--     | otherwise -> go cnt (progress config cols)

progress :: Config -> Array Int -> Array Int
progress config init = ST.run
  ( flip STArray.withArray init \arr -> do
      incNext <- ST.new true
      -- prev <- ST.new Nothing
      idx <- ST.new (-1)
      ST.while ((&&) <$> (ST.modify inc idx <#> (_ < config.colsNum)) <*> ST.read incNext) do
        i <- ST.read idx
        let { lower, upper } = config.colLimits !! i # fromMaybe' \_ -> unsafeCrashWith $ "progress: Array.index error at idx " <> show i
        elem <- unsafePartial $ STArray.Partial.peek i arr -- <#> fromMaybe' \_ -> unsafeCrashWith $ "progress: STArray peek error at idx " <> show i
        if elem < upper then do
          void $ STArray.modify i inc arr
          void $ ST.write false incNext
        else do
          newVal <- STArray.peek (i + 1) arr >>= pure <<< case _ of
            Just n ->
              let
                nextVal = n + 2
              in
                if nextVal < lower then lower else nextVal
            Nothing -> elem + 1
          void $ STArray.poke i newVal arr
          void $ ST.write true incNext

      overflow <- ST.read incNext
      -- en son eleman da son raddesine gelmişse kolonları boşalt ki
      -- üst katman işin bittiğini anlasın
      if overflow then
        ST.while
          (STArray.pop arr <#> isJust)
          (pure unit)
      else do
        void $ ST.write (config.colsNum - 1) idx
        ST.while (ST.modify dec idx <#> (_ >= 0)) do
          i <- ST.read idx
          this <- unsafePartial $ STArray.Partial.peek i arr
          when (this > config.maxCellValue - i) do
            next <- unsafePartial $ STArray.Partial.peek (i + 1) arr -- <#> fromMaybe' \_ -> unsafeCrashWith $ "progress: epilog STArray peek error at idx " <> show i
            void $ STArray.poke i (next + 1) arr

  -- ST.for 0 config.colsNum \i -> 
  --   STArray.modify i (const config.maxCellValue) arr
  )

inc :: Int -> Int
inc = (_ + 1)

dec :: Int -> Int
dec = (_ - 1)

valid :: forall r. { | LimitsConfigR r } -> Array Int -> Boolean
valid config columns =
  let
    predicates = [ {- fstLimit, lstLimit, -} consecPred ]
  in
    Array.all (_ $ columns) predicates
  where
  -- lstLimit = Array.head >>> fromMaybe' (\_ -> unsafeCrashWith "lstLimit Array.head hatası") >>> (_ >= config.lastElemBottomLimit)
  -- fstLimit = Array.last >>> fromMaybe' (\_ -> unsafeCrashWith "fstLimit Array.last hatası") >>> (_ <= config.firstElemTopLimit)
  consecPred cols = ST.run do
    arr <- STArray.thaw cols
    groupCount <- ST.new 0 -- ardışık grup sayısı <3 olmalı 
    prev <- ST.new =<< {- fromMaybe (unsafeCrashWith "consecPred STArray.peek hatası") <$> STArray.peek -}  STArray.pop arr
    consecCount <- ST.new 0
    let
      escPred = (\gc ad -> gc && ad)
        <$> (ST.read groupCount <#> (_ <= config.consecutiveGroupNumLimit)) -- ardışık grup sayısı <3 olmalı
        <*> (ST.read consecCount <#> abs >>> (_ < config.maxConsecutiveLimit)) -- ardarda iki kez ardışık bulmamalı
    ST.while
      ( (\nil esc -> nil && esc)
          <$> (STArray.length arr <#> (_ > 0)) -- dizinlide eleman olmalı
          <*> escPred
      )
      do
        mbElem <- STArray.pop arr
        mbPrev <- ST.read prev
        conCount <- ST.read consecCount
        let gcntModify = \_ -> when (conCount == 0) $ void $ ST.modify inc groupCount
        case mbPrev of
          _
            | mbPrev == (inc <$> mbElem) && conCount >= 0 -> do
                gcntModify unit
                void $ ST.modify inc consecCount
            | mbPrev == (dec <$> mbElem) && conCount <= 0 -> do
                -- when (conCount == 0) $ void $ ST.modify inc groupCount
                gcntModify unit
                void $ ST.modify dec consecCount
            -- gcntModify
            -- | conCount /= 0 ->  
            | otherwise -> ST.write 0 consecCount *> pure unit
        ST.write mbElem prev
    escPred

optInfo :: Config -> ParserInfo Config
optInfo config = info
  (optParser config <**> helper)
  ( fullDesc
      <> progDesc "Verilen kriterlere göre en şanslı kombinasyonları bulur"
      <> header (progName <> ": Şans oyunları analiz programı " <> version)
  )

globalOptsParser :: forall r. { | GlobalConfigR r } -> Parser { | GlobalConfigR () }
globalOptsParser config = ado
  colsNum <-
    option int $ long "sutun-sayisi"
      <> short 's'
      <> help "Oyun formundaki kolon sayısı (min: 6, max: 9)"
      <> metavar "TAMSAYI"
      <> showDefault
      <> value config.colsNum

  maxCellValue <-
    option int $ long "en-buyuk-hucre-degeri"
      <> short 'e'
      <> help "Kolonda seçilebilecek en büyük sayı (min:42, max:100)"
      <> metavar "TAMSAYI"
      <> showDefault
      <> value config.maxCellValue
  in { colsNum, maxCellValue }

limitOptsParser :: forall r. { | LimitsConfigR r } -> Parser { | LimitsConfigR () }
limitOptsParser config = ado
  maxConsecutiveLimit <-
    option int $ long "en-cok-ardisik-sayisi"
      <> short 'c'
      <> help "En çok ardışık gelebilecek değer sayısı"
      <> metavar "TAMSAYI"
      <> showDefault
      <> value config.maxConsecutiveLimit
  consecutiveGroupNumLimit <-
    option
      int $ long "en-cok-ardisik-grup-sayisi"
      <> short 'g'
      <> help "En çok ardışık grup sayısı"
      <> metavar "TAMSAYI"
      <> showDefault
      <> value config.consecutiveGroupNumLimit
  colLimits <-
    option
      colLimitsParser $ long "kolon-deger-limitleri"
      <> short 'l'
      <> help "Kolonlara girilebilecek değer aralıkları (virgülle ayrılmış 99-99 ya da 99- ya da -99 ya da - ya da boş değer)"
      <> metavar "DEGER_ARALIĞI_LİSTESİ"
      <> showDefault
      <> value config.colLimits
  in { consecutiveGroupNumLimit, maxConsecutiveLimit, colLimits }

colLimitRegex :: Regex
colLimitRegex = Regex.unsafeRegex "^(\\d{1,2})-(\\d{1,2})$" Regex.noFlags

colLimitsParser :: ReadM (Array { lower :: Int, upper :: Int })
colLimitsParser =
  eitherReader \s -> runExcept do
    let elems = String.trim <$> String.split (String.Pattern ",") s
    foldWithIndexM parseLimits [] elems
  -- (i → a → b → m a) → a → f b → m a
  where
  parseLimits idx result elem = do
    parsed <- case parseElem elem of
      Nothing -> throwError $ show idx <> ". kolon sınırlarını çözümlerken hata oluştu: '" <> elem <> "'"
      Just (NonEmptyArray [ _, Just l, Just u ]) | Just lower <- Int.fromString l, Just upper <- Int.fromString u -> pure { lower, upper }
      Just limits -> throwError $ show idx <> ". kolon sınırları için çözümlenemeyen değerler:" <> show limits
    pure $ parsed : result
  parseElem elem = Regex.match colLimitRegex elem

-- (i → b → a → b) → b → f a → b

colLimitsOK :: Config -> Either (Array String) Config
colLimitsOK config@{ maxCellValue, colLimits, colsNum } =
  ST.run do
    errors <- STArray.new
    let
      addError msg = void $ STArray.push msg errors
      colLimitsLen = Array.length colLimits
    limits <- STArray.thaw colLimits
    if (colLimitsLen /= colsNum) then
      addError $ "Kolon limiti sayısı (" <> show colLimitsLen <> ") kolon sayısına (" <> show colsNum <> ") eşit olmalıdır"
    else do
      idx <- ST.new colLimitsLen
      let
        checkLimit idx { upper, lower } =
          unless (upper >= lower) $ addError $ "alt sınır üst sınırdan büyük olamaz (alt=" <> show lower <> ",üst=" <> show upper <> ",dizin=" <> show idx <> ")"
        checkBounds idx value msg = do
          let
            maxValue = maxCellValue - idx
            minValue = colsNum - idx
          unless (value <= maxValue) $ addError $ msg <> " sınır (" <> show value <> ") olabilecek en üst sınırdan (" <> show maxValue <> ") büyük olamaz (dizin=" <> show idx <> ")"
          unless (value >= minValue) $ addError $ msg <> " sınır (" <> show value <> ") olabilecek en alt sınırdan (" <> show minValue <> ") küçük olamaz (dizin=" <> show idx <> ")"
        checkNeighbors idx elem prev = do
          unless (elem.upper > prev.upper) $ addError $ show idx <> ". sıradaki elemanın üst sınırı " <> show (idx + 1) <> ". sıradaki elemanın üst sınırından küçük ya da eşit olamaz"
          unless (elem.lower > prev.lower) $ addError $ show idx <> ". sıradaki elemanın alt sınırı " <> show (idx + 1) <> ". sıradaki elemanın alt sınırından küçük ya da eşit olamaz"
          unless (elem.upper >= prev.lower) $ addError $ show idx <> ". sıradaki elemanın üst sınırı " <> show (idx + 1) <> ". sıradaki elemanın alt sınırından küçük olamaz"
      ST.while (ST.modify dec idx <#> (_ >= 0)) do
        i <- ST.read idx
        mbelem <- STArray.peek i limits
        case mbelem of
          Nothing -> void $ STArray.push ("colLimitsOK: invalid index to peek: " <> show i) errors
          Just elem -> do
            checkLimit i elem
            checkBounds i elem.upper "üst"
            checkBounds i elem.lower "alt"
            mbprev <- STArray.peek (i + 1) limits
            maybe' (\_ -> pure unit) (checkNeighbors i elem) mbprev
    ifM (STArray.length errors <#> (_ > 0))
      (STArray.freeze errors <#> Left)
      (STArray.freeze limits <#> ((config { colLimits = _ }) >>> Right))

optParser :: Config -> Parser Config
optParser config = ado
  globalConfig <- globalOptsParser config
  limitsConfig <- limitOptsParser config
  command <- hsubparser $ fold
    [ command "say" $ info (pure Count) $ fullDesc <> progDesc "Bulunan kombinasyonların sayısını verir"
    , command "yaz" $ info
        ( Write <$>
            ( hsubparser $ fold
                [ command "çıkışa" $ info (pure Print) $ fullDesc <> progDesc "Bulunanları standart çıkışa yazar"
                , command "dosyaya" $ info
                    ( File <$>
                        (argument str $ help "Yazılacak dosyanın yolağı" <> metavar "YOLAK")
                    )
                    (fullDesc <> progDesc "Bulunanları yolağı verilen dosyaya yazar")

                ]
            )
        )
        (fullDesc <> progDesc "Kombinasyonları ekrana ya da verilen dosyaya yazar")
    ]
  in { command } `Record.merge` globalConfig `Record.merge` limitsConfig
