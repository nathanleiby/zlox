def decrement_loop(name, n):
  print(name)
  print(n)
  if (n == 1):
    return n
  x = n-1
  sum = decrement_loop("left", x) + decrement_loop("right", x)
  print("sum")
  print(sum)
  return sum

print(decrement_loop("start", 3))
