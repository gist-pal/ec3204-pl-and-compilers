{
  int size;
  int[100] a;
  int i;
  int j;
  int cnt;

  i = 0;
  cnt = 0;
  while (i<10) {
    j = 0;
    while (j<10) {
      a[i*10+j] = cnt;
      j++;
      cnt++;
    }
    i++;
  }
  print (a[10]);
  print (a[20]);
}
