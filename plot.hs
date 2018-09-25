import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import System.Random (randomRIO)
import Control.Monad (replicateM)

-- upper bound
theta :: Float
theta = 30.0

-- volume of samples
n :: Int
n = 100

-- number of samples per k
m :: Int
m = 10

-- number of experiments
maxk :: Int
maxk = 24

approxSamples :: Int -> Int -> Float -> Int -> IO [Float]
approxSamples m n theta k = replicateM m $ fmap (approxSample k) sample
    where sample = replicateM n $ fmap (** fromIntegral k) $ randomRIO (0.0, theta)
          approxSample k sample = ((fromIntegral k + 1) * sum sample / fromIntegral (length sample)) ** (1 / fromIntegral k)
    
graphIO :: IO [(Int, Float)]
graphIO = do 
        deviations <- traverse deviation [1..maxk] 
        return $ zip [1..maxk] deviations
    where deviation :: Int -> IO Float
          deviation k = do
              approximations <- approxSamples m n theta k
              return $ (1 / (fromIntegral m) * foldl1 (\sum theta' -> sum + (theta' - theta) ** 2.0) approximations) ** 0.5
        
main :: IO ()
main = do 
    graph <- graphIO    
    toFile def "approximation_deviation.png" $ do
    layout_title .= "Floats from [0, 30]"
    layout_x_axis . laxis_title .= "Power (k)"
    layout_y_axis . laxis_title .= "AQD (approximation deviation)"
    setShapes [PointShapeStar]
    plot (line "deviation" [ graph ])

