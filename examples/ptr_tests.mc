struct person{
    string name;
    int age;
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

void modify(Ptr<person> pers){
    (*pers).name = "abc";
    (*pers).age = 125;
    person a = *pers;
    a.name = "aass";
    a.age = 884;
}

void check_dereference(){
    person a;
    a.name = "abc";
    a.age = 12;
    Ptr<person> refp = &a;
    person b = *refp;
    b.name = "def";
    assert(a.name == "abc");
    assert(b.name == "def");
}

void check_modify_ptr(){
    person p;
    p.name = "def";
    p.age = 12;
    modify(&p);
    assert(p.name == "abc");
    assert(p.age == 125);
    check_dereference();
    check_mod_arr();
}

void ok_check_modify = check_modify_ptr();