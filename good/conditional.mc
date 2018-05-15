int check_ifs(){
    if(true){}
    else{ assert(false); }
    if(false){ assert(false); }
    else {}
    print("ok");
    return 1;
}

int res = check_ifs();