# RSA encryption

RSA encryption module written in Haskell 

Exported functions:

- ```generateKeyPair```: Generates a private, public key pair. Each have their own datatype, use ```privateKey```, ```publicKey```, ```returnE``` to get the values.
  Takes two arguments, the amount of checks for the Fermat Primality Test (e.g. 1024) and the bit size of the prime (e.g. 256).
- ```encrypt```: Encrypts a message, which has to be positive and smaller than the public key. 
  Takes two arguments, the message and the public key.
- ```decrypt```: Decrypts an encrypted message.
  Takes three arguments, the message, the private key and the public key.
- ```privateKey```: Returns the value of the private key.
  Takes the private key as argument.
- ```publicKey```: Returns the value of the public key.
  Takes the public key as argument.
- ```returnE```: Returns the *e* value used for the encrpytion (e.g. 65537).
  Takes the public key as argument.
  
Sources: https://en.wikipedia.org/wiki/RSA_%28cryptosystem%29#Key_generation
