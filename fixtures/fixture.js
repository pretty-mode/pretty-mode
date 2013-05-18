a=1;
b=2;
c=-1;
d=[1,2];

// Ordering

assert(1 << 2 == 4);
assert((0+1)<<(0+2)==4);
assert(1<<2==4);
assert(a<<b==4);

assert(16 >> 2 == 4);
assert((8+8)>>(1+1)== 4);
assert(16>>2==4);
assert(b>>a==1);

assert(1 <= 2);
assert((0+1)<=(1+1));
assert(1<=2);
assert(a<=b);

assert(2 >= 1);
assert((1+1)>=(0+1));
assert(2>=1);
assert(b>=a);

assert(-1 >>> 1 == 2147483647);
assert((2-1)>>>(0+1)==2147483647);
assert(-1>>>1== 2147483647);
assert(c>>>a==2147483647);

// Equality

assert(1 != 2);
assert(1!=2);
assert((0+1)!=(1+1));
assert(a!=b);

assert(1 == 1);
assert(1==1);
assert((1+1)==(1+1));
assert(a==a);

assert(1 === 1);
assert(1===1);
assert((1+1)===(1+1));
assert(a===a);

// Logic

assert(!false);
assert(!(false));
assert(!0);

assert(true && true);
assert(true&&true);
assert(a&&b);
assert((true)&&(true));

assert(false || true);
assert(false||true);
assert((false)||(true));
assert(a||b);

// Sets

assert(1 in [1,2]);
assert((1)in([1,2]));

// Subscripts

a(= [0,1,2,3,4]);
assert(a[0]==0);
assert(a[1]==1);
assert(a[2]==2);
assert(a[3]==3);
assert(a[4]==4);

// Null

assert(null == null);
assert((null)==(null));
assert(null==null);

// Arithmetic

assert(Math.sqrt 16 == 4);

a = 0

assert(++a == 1);
assert((++a)==2);

assert(a++ == 2);
assert(a++==3);
assert((a++)==4);

// Undefined

assert(undefined == undefined);
assert((undefined)==(undefined));
assert(undefined==undefined);

assert(undefined == void 0);
assert((undefined)==(void 0));
assert(undefined==void 0);
