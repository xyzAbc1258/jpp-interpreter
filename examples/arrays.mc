Array<int> test_array(){
    Array<int> arr = new int[13];
    for(int i = 0; i < length(arr); i = i +1){
        arr[i] = i*i;
    }
    for(int i = 0; i < length(arr); i = i +1){
        print(toString(i) + " squared = " + toString(arr[i]));
        assert(arr[i] == (i*i));
    }
    return arr;
}

Array<int> arr = test_array();