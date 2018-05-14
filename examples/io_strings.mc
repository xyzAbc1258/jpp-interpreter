int main(){
    print("Hello, what's your name ?");
    string name = readString();
    Array<char> chared = toCharArray(name);
    string unChared = fromCharArray(chared);
    print ("Hello " + unChared);
    for(int i = 0; i < length(chared); i = i + 1){
        print(toString(chared[i]));
    }
    print("How old are you ?");
    int age = readInt();
    if(age < 18){
        print(toString(age) + "? " + "You are underage!!!");
    }
    else{
        print(toString(age) + "? " + "You are old :P ");
    }
    return 1;
}

int res = main();