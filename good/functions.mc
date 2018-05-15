// after return nothing else is executed
int fact_iter(int i){
  if (i <= 1) { return 1;}
  int result = 1;
  for (int l = 2; true; l = l+1){
    result = result * l;
    if(l == i) { break; }
  }
  return result;
  assert(false);
}

// return break while
int fact_iter2(int i){
  int result = 1;
  print("Factorial " + toString(i));
  while(true) {
    if(i < 2) {
      return result;
      assert(false);
    }
    result = result * i;
    i = i -1;
  }
  return 0;
  assert(false);
}

// return stop function execution;
int fact_arr(int i){
  if(i < 0) {return 1;}
  Array<int> res = new int[i + 1];
  res[0] = 1;
  for(int l = 1; l <= i; l = l +1){
    res[l] = res[l - 1] * l;
  }
  return res[i];
  assert(false);
}

// recursive
int fact_rec(int i){
  if(i > 1){ return i * fact_rec(i -1); }
  return 1;
}

// functions as parameters
int fact_gen(Func<int, int> f, int i){
  if(i > 1){ return i * f(i -1); }
  return 1;
}

int global;

int modify_global(){
    int cons = 125896;
    global = cons;
    return cons;
}

void assert_global_modified(){
    assert(modify_global() == global);
}

int assert_correct(){
  assert (fact_iter(5) == 120);
  assert (fact_gen(fact_iter2, 6) == 720);
  assert (fact_rec(6) == 720);
  assert (fact_arr(7) == 5040);
  assert_global_modified();
  print("factorial assertions ok");
  return 1;
}

int res = assert_correct();
