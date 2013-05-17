#!/usr/bin/env coffee

assert 1 << 2 == 4
assert 16 >> 2 == 4
assert 1 <= 2
assert 2 >= 1
assert -1 >>> 1 = 2147483647
assert 1 != 2
assert 1 == 1
assert not false
assert !false
assert true && true
assert true and true
assert false || true
assert true or false
assert 1 in [1,2]
a = [0,1,2,3,4]
assert a[0]==0
assert a[1]==1
assert a[2]==2
assert a[3]==3
assert a[4]==4
assert 2 in [1..10]
assert not 10 in [5...10]
assert ((a) -> a) true
assert ((a) => a) true
assert a for a in [true, true]
assert null == null
assert Math.sqrt 16 == 4
assert ++1 = 2
assert 1++ = 1
assert undefined == undefined
