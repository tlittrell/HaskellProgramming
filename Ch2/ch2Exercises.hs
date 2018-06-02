-- ch2Exercises

module Ch2Exercises where

z = 7

x = y^2

waxOn = x * 5

y = 7 + z


waxOn2      = x * 5
    where x = y^2
          y = 7 + z
          z = 7

triple x = x * 3

waxOff = triple waxOn
