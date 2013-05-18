#!/usr/bin/env coffee

require 'assert'

a=1
b=2
c=-1
d=[1,2]

# Ordering

assert 1 << 2 == 4
assert (0+1)<<(0+2)==4
assert 1<<2==4
assert a<<b==4

assert 16 >> 2 == 4
assert (8+8)>>(1+1)== 4
assert 16>>2==4
assert b>>a==1

assert 1 <= 2
assert (0+1)<=(1+1)
assert 1<=2
assert a<=b

assert 2 >= 1
assert (1+1)>=(0+1)
assert 2>=1
assert b>=a

assert -1 >>> 1 == 2147483647
assert (2-1)>>>(0+1)==2147483647
assert -1>>>1== 2147483647
assert c>>>a==2147483647

# Equality

assert 1 != 2
assert 1!=2
assert (0+1)!=(1+1)
assert a!=b

assert 1 == 1
assert 1==1
assert (1+1)==(1+1)
assert a==a

# Logic

assert not false
assert not(false)

assert !false
assert !(false)

assert true && true
assert true&&true
assert a&&b
assert (true)&&(true)

assert true and true
assert (true)and(true)

assert false || true
assert false||true
assert (false)||(true)
assert a||b

assert true or false
assert (true)or(false)

# Sets

assert 1 in [1,2]
assert (1)in([1,2])
assert 1in[1,2]

# Subscripts

a = [0,1,2,3,4]
assert a[0]==0
assert a[1]==1
assert a[2]==2
assert a[3]==3
assert a[4]==4

# Punctuation

a = 1
b = 10
c = 5

assert 2 in [1 .. 10]
assert 2 in [1..10]
assert 2 in [(1+1)..(9+9)]
assert 2 in [a..b]

assert not 10 in [5 ... 10]
assert not 10 in [5...10]
assert not 10 in [(5)...(10)]
assert not 10 in [c...b]

assert typeof String::substr == 'function'

# Arrows

assert ((a) -> a) true
assert ((a)->a) true

assert ((a) => a) true
assert ((a)=>a) true

# Quantifiers

assert a for a in [true, true]
assert 1for a in [true, true]

# Nil

assert null == null
assert (null)==(null)
assert null==null

# Arithmetic

assert Math.sqrt 16 == 4

a = 0

assert ++a == 1
assert (++a)==2

assert a++ == 2
assert a++==3

# Undefined

assert undefined == undefined
assert (undefined)==(undefined)
assert undefined==undefined
