void test_while(){
    int i = 0;
    while(i < 5){
        i = i + 1;
    }
    assert(i == 5);
}

void test_for(){
    Array<int> arr = new int[4];
    for(int i = 0; i < 4; i = i + 1){
        arr[i] = i*i;
    }
    assert(arr[0] == 0 && arr[1] == 1 && arr[2] == 4 && arr[3] == 9);
}

void test_break_while(){
    int bound = 12;
    int i = 0;
    while(true){
        i = i + 1;
        if(i == bound){
            break;
        }
        assert(i < bound);
    }
    assert(i == bound);
}

void test_break_for(){
    int i = 0;
    for(; i < 5; i = i + 1){
        break;
        assert(false);
    }
    assert(i == 0);
}

void test_continue_while(){
    int i = 0;
    while(i < 5){
        i = i + 1;
        continue;
        assert(false);
    }
}

void test_continue_for(){
    for(int i = 0;  i < 12; i = i +1){
        continue;
        assert(false);
    }
}

void test_all(){
    test_for();
    test_while();
    test_break_for();
    test_break_while();
    test_continue_for();
    test_continue_while();
}

void r = test_all();