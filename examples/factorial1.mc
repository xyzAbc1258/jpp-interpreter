struct error{
 int line;
 string msg;
}

 int fact_iter(int i){
  error e;
  if (i <= 1) { return 1;}
  int result = 1;
  for (int l = 2; l <= i; l = l+1){
    result = result * l;
  }
  return result;
}

int v = 0;

 int fact_iter2(int i){
  int result;
  while(true) {
    if(i < 2){ return result; }
    result = result * i;
    i = i -1;
  }
}

int fact_arr(int i){
  if(i < 0) {return 1;}
  int[] res = int[i + 1];
  res[0] = 1;
  for(int l = 1; l <= i; l = l +1){
    res[l] = res[l - 1] * l;
  }
  return res[i];
}

 int fact_rec(int i){
  if(i <= 1){ return 1;}
  return i * fact_rec(i - 1);
}

 int fact_gen(Func<int, int> f, int i){
  if(i <= 1){ return 1;}
  return i * f(i -1);
}

 void assert_correct(){
	assert (fact_iter(5) == 120);
	assert (fact_rec(6) == 720);
	assert (fact_gen(fact_iter, 6) == 720);
}

 void do_nothing(){
  while(true){
    assert_correct();
  }
}
