string s;

void do_sth(){
    s = "abc";
    int s = 125;
    assert(s == 125);
    for(int i = 0; i < 12; i = i +1 ){
        bool s = false;
        assert(s == false);
    }
    assert(s == 125);
}

int check_shadowing(){
    do_sth();
    assert(s == "abc"); 
    return 1;
}

int d = check_shadowing();