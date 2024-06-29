list<int> sortByFirst(list<int> l) {
    if (l -> empty()) {
        return list<int> {};
    }
    int middle = l -> front();
    l -> pop_front();
    list<int> left, right;

    while (!l -> empty()) {
        int next = l -> front();
        l -> pop_front();

        if (next < middle) {
            left -> push_back(next);
        } else {
            right -> push_back(next);
        }
    }

    left = sortByFirst(left);
    right = sortByFirst(right);

    left -> push_back(middle);
    while (!right -> empty()) {
        left -> push_back(right -> front());
        right -> pop_front();
    }
    return left;
}

int main() {
    list<int> in1 = list<int> {5, 3, 92, 1, -14, 2, 8};
    list<int> in2 = list<int> {6, 5, 4, 3, 2, 1};
    cout << sortByFirst(in1) << "\n";
    cout << sortByFirst(in2) << "\n";
    return 0;
}