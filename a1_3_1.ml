let rec gcd x y =
  if x = y then x
  else if x = 0 then y
  else if y = 0 then x
  else if x < 0 then gcd (-x) y
  else if y < 0 then gcd x (-y)
  else if x > y then gcd (x - y) y
  else if x < y then gcd x (y - x)
  else 0;