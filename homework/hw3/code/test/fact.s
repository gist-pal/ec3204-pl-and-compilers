{
  int n;
  int i;
  int s;
  int[10] a;
  
  i = 1;
  s = 1; 
  read (n); 
  while (i <= n) {
    s = s * i;
    i++;
  } 
  print (s);
}
