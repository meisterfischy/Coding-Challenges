module RSAEncryption (generateKeyPair, encrypt, decrypt, privateKey, publicKey, returnE) where 


import System.Random
import Control.Monad (replicateM)


newtype PrivateKey = PrivateKey Integer deriving (Show)
data PublicKey     = PublicKey Integer Integer deriving (Show) -- first argument: public key, second argument: value e 


privateKey :: PrivateKey -> Integer 
privateKey (PrivateKey key) = key


publicKey :: PublicKey -> Integer 
publicKey (PublicKey key _) = key


returnE :: PublicKey -> Integer 
returnE (PublicKey _ e) = e


-- Every prime p satisfies a^(p-1) (mod p) = 1 for all a in [1,p-1]
-- Takes a number p and the value of the base a 
fermatPrimalityTest :: Integer -> Integer -> Bool
fermatPrimalityTest p a = powerMod a (p-1) p == 1


-- Calculates x^e (mod m) more efficiently: O(log^2 m * log m)
powerMod :: Integer -> Integer -> Integer -> Integer
powerMod x e m = helper x e m 1
    where helper _ 0 _ result = result
          helper x e m result | even e    = helper (x^2 `mod` m) (e `div` 2) m result
                              | otherwise = helper x (e-1) m (result*x `mod` m)


-- Determines whether a number is probably a prime, using Fermat Primality Test
-- Arguments:
--   p: The number, which is to be checked 
--   randList: A list of random integers in the range [1,p-1]
probablyPrime :: Integer -> [Integer] -> Bool
probablyPrime p randList | p == 2 = True
                         | even p = False
                         | p < 2  = False
                         | otherwise = all (\x -> powerMod x (p-1) p == 1) randList


-- Returns a list of random values in the range [1,bound]
randList :: Int -> Integer -> IO [Integer]
randList len bound = replicateM len (randomRIO (1,bound))


-- Returns a prime of variable bit size
-- Arguments:
--   checks: The amount of checks for the Fermat Primality Test 
--   size: The bit size of the prime
getPrime :: Int -> Integer -> IO Integer
getPrime checks size = do
    g <- newStdGen
    let toCheckList = randomRs (2^(size-1)+1, 2^size-1 :: Integer) g
    list <- randList checks (2^size-1)
    let p = dropWhile ((==False) . \x -> probablyPrime x list) toCheckList
    return $ head p


-- Returns the p q primes for RSA 
-- Arguments:
--   checks: The amount of checks for the Fermat Primality Test 
--   size: The bit size of the prime
getRSAPrimes :: Int -> Integer -> IO (Integer, Integer)
getRSAPrimes checks size = do
    p <- getPrime checks size
    q <- getPrime checks size
    return (p,q)


leastCommonMultiple :: Integer -> Integer -> Integer
leastCommonMultiple a b = abs (a*b) `div` gcd a b


-- Takes two integers a b and computes the coefficients s.t. ax+bx=gcd(a,b)
extendedEuclideanAlgorithm :: Integer -> Integer -> (Integer,Integer)
extendedEuclideanAlgorithm a b = helper (a,b) (1,0) (0,1)
    where helper (old_r, r) (old_s, s) (old_t, t)
            | r == 0    = (old_s, old_t)
            | otherwise = helper (r, old_r - quotient * r) (s, old_s - quotient * s) (t, old_t - quotient * t)
            where quotient = old_r `div` r



-- Generates a private, public key pair for RSA 
-- Arguments:
--   checks: The amount of checks for the Fermat Primality Test 
--   size: The bit size of the prime
generateKeyPair :: Int -> Integer -> IO (PrivateKey, PublicKey)
generateKeyPair checks size = do
    (p,q) <- getRSAPrimes checks size
    let lcm   = leastCommonMultiple (p-1) (q-1)
    let (d,_) =  extendedEuclideanAlgorithm e lcm
    return (PrivateKey (d `mod` lcm) , PublicKey (p*q) e)
        where e = 65537


-- Encrypts a message encoded as an integer with a public key
-- Arguments:
--   msg: Message to be encrypted, has to be smaller than the pub key and positive 
encrypt :: Integer -> PublicKey -> Integer
encrypt msg (PublicKey pub e) = powerMod msg e pub


-- Decrypts an encrypted message
decrypt :: Integer -> PrivateKey -> PublicKey -> Integer
decrypt msg (PrivateKey priv) (PublicKey pub _) = powerMod msg priv pub
