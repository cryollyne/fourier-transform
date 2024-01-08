import Data.Complex


fft' :: [Complex Double] -> Complex Double -> [Complex Double]
fft' [a] _ = [a]
fft' coeff w = l1 ++ l2
    where
    len = length coeff
    (evens, odds) = splitEvens coeff
    w' = w^(2 :: Int)
    yevens = fft' evens w'
    yodds = fft' odds w'
    l1 = [ yevens!!(n-1) + (w^n)*yodds!!(n-1) | n <- [1.. (div len 2) ]]
    l2 = [ yevens!!(n-1) - (w^n)*yodds!!(n-1) | n <- [1.. (div len 2) ]]

fft :: [Complex Double] -> [Complex Double]
fft coeff = reverse . fft' coeff $ w
    where
    w = mkPolar 1.0 (2.0*pi/(fromIntegral . length $ coeff :: Double))


ifft :: [Complex Double] -> [Complex Double]
ifft coeff = reverse . fmap (/(fromIntegral . length $ coeff:: Complex Double)) $ fft' coeff w
    where
    w =  mkPolar 1.0 ( -2.0*pi / (fromIntegral . length $ coeff:: Double))

splitEvens :: [a] -> ([a], [a])
splitEvens [] = ([], [])
splitEvens [a] = ([a], [])
splitEvens (x:y:ys) = (x:evens, y:odds)
    where
    (evens,odds) = splitEvens ys
