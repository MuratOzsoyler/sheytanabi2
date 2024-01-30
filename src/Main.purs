module Main where

import Prelude

import Control.Monad.ST (run, while) as ST
import Control.Monad.ST.Ref (modify, new, read, write) as ST
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Foldable (fold, for_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (abs)
import Data.Show.Generic (genericShow)
import Data.String as String
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Options.Applicative (Parser, ParserInfo, argument, command, execParser, fullDesc, header, help, helper, hsubparser, info, int, long, metavar, option, progDesc, short, showDefault, str, value, (<**>))
import Partial.Unsafe (unsafeCrashWith)
import Type.Row (type (+))

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
  ( lastElemTopBottomLimit :: Int
  , firstElemTopLimit :: Int
  , consecutiveGroupNumLimit :: Int
  , maxConsecutiveLimit :: Int
  | r
  )

type GlobalConfig = { | GlobalConfigR () }

type Config = { command :: Command | GlobalConfigR + LimitsConfigR + () }

type State = Array Int

defaultConfig =
  { colsNum: 6
  , maxCellValue: 60
  , command: Noop
  , lastElemTopBottomLimit: 16
  , firstElemTopLimit: 46
  , consecutiveGroupNumLimit: 2
  , maxConsecutiveLimit: 2
  } :: Config

main :: Effect Unit
main = do
  config <- execParser $ optInfo defaultConfig
  logShow defaultConfig
  logShow config

  findResult config $ Array.replicate config.colsNum 1

findResult :: Config -> State -> Effect Unit
findResult config initial = do
  cnt <- go 0 initial
  case config.command of
    Count -> Console.log $ "Seçilen kombinasyon sayısı: " <> show cnt
    _ -> pure unit
  where
  go cnt = case _ of
    [] -> pure cnt
    cols
      | valid cols -> do
          let textCols = String.joinWith ";" $ map show $ Array.reverse cols
          case config.command of
            Noop -> go cnt []
            Count -> go (cnt + 1) (progress cols)
            Write Print -> do
              Console.log textCols
              go (cnt + 1) (progress cols)
            Write (File path) -> do
              FS.appendTextFile UTF8 path textCols
              go (cnt + 1) (progress cols)
      | otherwise -> go cnt (progress cols)
    where
    valid arr =
      let
        predicates = [ fstLimit, lstLimit, consecPred ]
      in
        Array.all (_ $ arr) predicates
    lstLimit = Array.head >>> fromMaybe (unsafeCrashWith "lstLimit Array.head hatası") >>> (_ <= config.lastElemTopBottomLimit)
    fstLimit = Array.last >>> fromMaybe (unsafeCrashWith "fstLimit Array.last hatası") >>> (_ >= config.firstElemTopLimit)
    consecPred cols = ST.run do
      arr <- STArray.thaw cols
      gcnt <- ST.new 0 -- ardışık grup sayısı <3 olmalı 
      prev <- ST.new =<< {- fromMaybe (unsafeCrashWith "consecPred STArray.peek hatası") <$> STArray.peek -}  STArray.pop arr
      ascDsc <- ST.new 0
      let
        escPred = (\gc ad -> gc && ad)
          <$> (ST.read gcnt <#> (_ <= config.consecutiveGroupNumLimit)) -- ardışık grup sayısı <3 olmalı
          <*> (ST.read ascDsc <#> abs >>> (_ < config.maxConsecutiveLimit)) -- ardarda iki kez ardışık bulmamalı
      ST.while
        ( (\nil esc -> nil && esc)
            <$> (STArray.length arr <#> (_ > 0)) -- dizinlide eleman olmalı
            <*> escPred
        )
        do
          mbe <- STArray.pop arr
          mbp <- ST.read prev
          ad <- ST.read ascDsc
          let gcntModify = \_ -> when (ad == 0) (void $ ST.modify inc gcnt)
          case mbp of
            _
              | mbp == (inc <$> mbe) && ad >= 0 -> do
                  void $ ST.modify inc ascDsc
                  -- when (ad == 0) (void $ ST.modify inc gcnt)
                  gcntModify unit
              | mbp == (dec <$> mbe) && ad <= 0 -> do
                  void $ ST.modify dec ascDsc
                  -- when (ad == 0) (void $ ST.modify inc gcnt)
                  gcntModify unit
              -- | ad /= 0 ->  
              | otherwise -> pure unit
      escPred
    progress init = ST.run
      ( flip STArray.withArray init \arr -> do
          incNext <- ST.new true
          idx <- ST.new 0
          ST.while
            do
              i <- ST.read idx
              inxt <- ST.read incNext
              pure $ i < config.maxCellValue && inxt
            do
              i <- ST.read idx
              mbe <- STArray.peek i arr
              for_ mbe \e -> do
                if e < config.maxCellValue then do
                  void $ STArray.modify i inc arr
                  void $ ST.write false incNext
                else do
                  void $ STArray.poke i 1 arr
                  void $ ST.write true incNext
          -- en son eleman da son raddesine gelmişse kolonları boşalt ki
          -- üst katman işin bittiğini anlasın
          whenM (ST.read incNext)
            $ ST.while
                (STArray.pop arr <#> isJust)
                (pure unit)
      -- ST.for 0 config.colsNum \i -> 
      --   STArray.modify i (const config.maxCellValue) arr
      )
    inc = (_ + 1)
    dec = (_ - 1)

optInfo :: Config -> ParserInfo Config
optInfo config = info
  (optParser config <**> helper)
  ( fullDesc
      <> progDesc "Verilen kriterlere göre en şanslı kombinasyonları bulur"
      <> header "Şans oyunları analiz programı"
  )

globalOptsParser :: Config -> Parser GlobalConfig
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

optParser :: Config -> Parser Config
optParser config = ado
  { colsNum, maxCellValue } <- globalOptsParser config
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

  in config { colsNum = colsNum, maxCellValue = maxCellValue, command = command }
