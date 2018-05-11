int fact_iter(int i){
  if (i <= 1) { return 1;}
  int result = 1;
  for (int l = 2; true; l = l+1){
    result = result * l;
    if(l == i) { break; }
  }
  return result;
}

int v;

int fact_iter2(int i){
  int result = 1;
  print("Factorial " + toString(i));
  while(true) {
    if(i < 2) {return result;}
    result = result * i;
    i = i -1;
  }
  return 0;
}

int fact_arr(int i){
  if(i < 0) {return 1;}
  Array<int> res = new int[i + 1];
  res[0] = 1;
  for(int l = 1; l <= i; l = l +1){
    res[l] = res[l - 1] * l;
  }
  return res[i];
}

int fact_rec(int i){
  if(i > 1){ return i * fact_rec(i -1); }
  return 1;
}

int fact_gen(Func<int, int> f, int i){
  if(i > 1){ return i * f(i -1); }
  return 1;
}

int assert_correct(){
	assert (fact_iter(5) == 120);
	assert (fact_gen(fact_iter2, 6) == 720);
  assert (fact_rec(6) == 720);
  assert (fact_arr(7) == 5040);
  print("factorial assertions ok");
	return 1;
}

int fc(Func<int,int> f, int a){
  print("Arg :" + toString(a));
  int res = f(a);
  print("Res: " + toString(res));
  return res;
}

bool check_age(int age){
  return age > 18;
}

int main(){
  print("Hello World." + " What's your name ?");
  string name = readString();
  print("Hello " + name + ". How old are you");
  if(check_age(readInt())){
    print("Hmm... You're quite old :P");
  }
  else {
    print("You're still young");
  }
  
  return 0;
}

int check_length(){
  int res = length(new int[12]);
  print(res);
  return res;
}

struct Person{
  int age;
  string name;
}


// sprawdzenie przekzaywania przez wartość
void print_person(Person ps){
  print("Osoba " + ps.name + " w wieku " + toString(ps.age));
}

Person modifyPerson(Person ps){
  ps.age = 12;
  ps.name = "Adam";
  print_person(ps);
  return ps;
}

int check_passing_structs(){
  Person p;
  p.age = (0-1) * 125;
  p.name = "Janusz";
  // deep copy
  Person p1 = p;
  p1.name = "Krzysztof";
  print("Kopiowany name: " + p1.name + " oryginalny name: " + p.name);
  print_person(p);
  Person np = modifyPerson(p);
  print("po modyfikacji");
  print_person(p);
  print("zamodyfikowana osoba");
  print_person(np);
  return 1;
}

string stringify(Func<int,string> f, int a){
  return f(a);
}


void print_alphabet(){
  Array<char> letters = new char[26];
  for(int i = 0; i < 26; i = 1+i){
    letters[i] = itoc(97 + i);
  }
  string result;
  for(int i = 0; i < 26; i = i +1){
    result = result + toString(letters[i]);
  }
  print(result);
}


Array<Person> dump_array(Array<Person> arr){
  for(int i = 0; i < length(arr); i = i +1){
    arr[i].age = i;
    arr[i].name = "name" + toString(i);
    print("arr[" + toString(i) + "] :");
    print_person(arr[i]);
  }
  return arr;
}

void do(){
  print("should be");
  return;
  assert(false);
}

int fff(){
  do();
  return 1;
}

void check_break(){
  int i = 0;
  while(true){
    if(i == 5) { break; }
    i = i +1;
  }
  assert(i == 5);
}

void check_continue(){
  for(int i = 0; i < 3; i = i +1){
    continue;
    assert(false);
  }
  print("continue ok");
}

int check_div0(){
  return 2/0;
}

void return_in_while(){
  while(true){
    return;
    assert(false);
  }
  assert(false);
}

void do_n(){
  if(true){
    //break;
  }
  else{
    continue;
  }
}

int f = fff();
int as = assert_correct();
void alph = print_alphabet();
void cb = check_break();
void cc = check_continue();
//int rr = check_div0();
void ff = return_in_while();
//Person p;
//int a = p.age;
//string s = p.name;
//int res = check_passing_structs();
//Array<Person> persArr = dump_array(new Person[12]);
//Person p2 = p;