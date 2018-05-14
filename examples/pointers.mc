int modify_int(Ptr<int> ptr){
    int const = 2114565;
    (*ptr) = const;
    return const;
}

void assert_modify_ptr(){
    int i = 0;
    int c = modify_int(&i);
    assert(i == c);
}

struct Person{
    int age;
    string name;
}

void passing_part_of_struct(){
    Person p;
    int c = modify_int(& p.age); // dot binds stronger
    assert(p.age == c);
}

Person modif_Person(Ptr<Person> ptr){
    Person np;
    np.age = 125;
    np.name = "abc";
    (*ptr).age = 125;
    (*ptr).name = "abc";
    return np;
}

void assert_modify_struct_ptr(){
    Person p;
    Person np = modif_Person(&p);
    assert(p.age == np.age && p.name == np.name);
}

void mod_arr(Ptr<Array<int>> ptrarr){
    for(int i = 0; i < length(*ptrarr); i = i + 1){
        (*ptrarr)[i] = i;
    }
}

void check_mod_arr(){
    Array<int> arr = new int[12];
    mod_arr(&arr);
    for(int i = 0; i < length(arr); i = i + 1){
        assert(arr[i] == i);
    }
}

void modify(Ptr<Person> pers){
    (*pers).name = "abc";
    (*pers).age = 125;
    Person a = *pers;
    a.name = "aass";
    a.age = 884;
}

void check_dereference(){
    Person a;
    a.name = "abc";
    a.age = 12;
    Ptr<Person> refp = &a;
    Person b = *refp;
    b.name = "def";
    assert(a.name == "abc");
    assert(b.name == "def");
}

void check_modify_ptr(){
    Person p;
    p.name = "def";
    p.age = 12;
    modify(&p);
    assert(p.name == "abc");
    assert(p.age == 125);
    check_dereference();
    check_mod_arr();
}

int assert_ptrs(){
    assert_modify_ptr();
    check_modify_ptr();
    assert_modify_struct_ptr();
    return 1;
}

int r = assert_ptrs();