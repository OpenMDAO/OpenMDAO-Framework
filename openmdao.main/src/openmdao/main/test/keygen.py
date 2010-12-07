import time

from Crypto.PublicKey import RSA
from Crypto.Random import get_random_bytes

for i in range(100):
    start = time.time()
    key_pair = RSA.generate(2048, get_random_bytes)
    et = time.time() - start
    print 'key generated in %.2f' % et
