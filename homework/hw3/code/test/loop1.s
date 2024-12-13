/* Fibonacci number */
{
  int x;
  int y;
  int n;
  int z;
  int i;
  int[10] fib;
  x = 0;
  y = 1;
  z = 0;
  i = 0;
  n = 10;
  while (i < n) {
    y = z + y;
    z = x;
    x = y;
    fib[i] = y;
    i++;
  }
  i = 0;
  while (i < n) {
    print (fib[i]);
    i++;
  }
}
