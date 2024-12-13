/* Fibonacci number */
{
  int x;
  int y;
  int z;
  int n;
  int i;
  x = 0;
  y = 1;
  z = 0;
  i = 0;
  read (n); 
  while (i < n) {
    y = z + y;
    z = x;
    x = y;
    i++;
  }
  print (y);
}
