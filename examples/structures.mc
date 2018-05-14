struct BirthInfo{
    string cityName;
    string dateOfBirth;
}
struct Person {
    int age;
    string name;
    BirthInfo birthInfo;
}

Person create_person(){
    Person p;
    p.name = "abc";
    p.age = 12;
    p.birthInfo.cityName = "Warsaw";
    p.birthInfo.dateOfBirth = "2018-05-14";
    assert(p.name == "abc");
    assert(p.age == 12);
    assert(p.birthInfo.cityName == "Warsaw");
    return p;
}

void assignment_by_value(){
    Person p = create_person();
    Person s = p;
    s.age = p.age + 12;
    s.birthInfo.cityName = "Radom";
    assert(s.age == (p.age + 12));
    assert(p.birthInfo.cityName != "Radom");
}
// to fix, assignment of void
void doo = assignment_by_value();