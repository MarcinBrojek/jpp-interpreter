bool vempty(list<int> &vector) {
    return vector->empty();
}

void vpush_back(list<int> &vector, int elem) {
    vector->push_back(elem);
}

void vpop_back(list<int> &vector) {
    if (vector->empty()) {
        return;
    }
    vector->pop_back();
}

int vsize(list<int> &vector) {
    if (vector->empty()) {
        return 0;
    }
    int tmp = vector->front();
    vector->pop_front();
    int len = 1 + vsize(vector);
    vector->push_front(tmp);
    return len;
}

tuple<int, bool> vget(list<int> &vector, int index) {
    if (vector->empty() || index < 0) {
        return make_tuple(0, false);
    }
    if (index == 0) {
        return make_tuple(vector->front(), true);
    }
    int tmp = vector->front();
    vector->pop_front();
    tuple<int, bool> res = vget(vector, index - 1);
    vector->push_front(tmp);
    return res;
}

void vset(list<int> &vector, int index, int value) {
    if (vector->empty() || index < 0) {
        return;
    }
    if (index == 0) {
        vector->pop_front();
        vector->push_front(value);
        return;
    }
    int tmp = vector->front();
    vector->pop_front();
    vset(vector, index - 1, value);
    vector->push_front(tmp);
}

list<int> vector(int default_value, int vsize) {
    list<int> res;
    for (int i = 0; i < vsize; i++) {
        res->push_back(default_value);
    }
    return res;
}

int main() {
    list<int> vector1 = list<int> {0, 1, 2, 3};
    list<int> vector2 = vector(0, 4);
    for (int i = 0; i < 4; i++) {
        vset(vector2, i, i);
    }

    bool res = true;
    int failed_index = 0;
    for (int i = 0; i < 4; i++) {
        res &&= vget(vector1, i) == vget(vector2, i) && vget(vector1, i) == make_tuple(i, true);
        if (!res) {
            failed_index = i;
            break;
        }
    }
    if (res) {
        cout << "vector1 == vector2\n" << vsize(vector1) == vsize(vector2) << "\n";
    } else {
        cout << "Vectors are not equal: " << vget(vector1, failed_index) << " != "
        << vget(vector2, failed_index) << "\n";
    }

    cout << vector1 << " " << vector2 << "\n";

    while(!vempty(vector1)) {
        vpop_back(vector1);
    }

    while(!vempty(vector2)) {
        vpop_back(vector2);
    }

    cout << vector1 << " " << vector2 << "\n";
    cout << vget(vector1, 2);

    return 0;
}