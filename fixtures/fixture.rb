#!/usr/bin/env ruby

require 'test/unit'

# punctuation

class Main < Test::Unit::TestCase
  def test_punctuation
    a = 1
    b = 10
    [1 .. 10]
    [1 ... 10]
    [1..10]
    [1...10]
    [(1)..(10)]
    [(1)...(10)]
    [a..b]
    [a...b]
  end

  def test_main
    a=1
    b=2

    # bitshift

    assert_equal 1 << 2, 4
    assert_equal (1)<<(2), 4
    assert_equal 1<<2, 4

    assert_equal 4 >> 1, 2
    assert_equal (4)>>(1), 2
    assert_equal 4>>1, 2

    # ordering

    assert 2 >= 1
    assert 2>=1
    assert (2)>=(1)
    assert b>=a

    assert 1 <= 2
    assert 1<=2
    assert (1)<=(2)
    assert a<=b

    # equality

    assert 1 == 1
    assert 1==1
    assert (1)==(1)
    assert a==a

    assert 1 != 2
    assert 1!=2
    assert (1)!=(2)
    assert a!=b

    assert Fixnum === 1
    assert Fixnum===1
    assert (Fixnum)===(1)
    assert Fixnum===a

    assert 'abc' =~ /abc/
    assert 'abc'=~/abc/
    assert ('abc')=~(/abc/)

    assert 'abc' !~ /cba/
    assert 'abc'!~/cba/
    assert ('abc')!~(/cba/)

    # logic

    # assert(not false)
    assert(not(false))

    assert ! false
    assert !false
    assert !(false)

    assert true and true
    assert (true)and(true)

    assert((false or true))
    assert(((false)or(true)))

    assert true && true
    assert true&&true
    assert (true)&&(true)

    assert false || true
    assert false||true
    assert (false)||(true)

    # arrows

    assert({:a => true}[:a])
    assert({:a=>true}[:a])
    assert({(:a)=>(true)}[:a])

    f = -> a { a**2 }
    f=->a{ a**2 }
    f = ->(a) { a**2 }
    assert_equal f.(3), 9

    # function

    f = lambda {|a| a**2}
    f=lambda{|a| a**2}
    assert_equal f.(4), 16

    # nil

    assert_nil nil
    assert_nil (nil)

  end
end
