int main() {
    list<int> l;

    l->push_back(1);
    l->push_back(2);
    l->push_back(3);

    if (l -> front() != 1) {
        cout << "1 is not front\n";
    }
    if (l -> back() != 3) {
        cout << "3 is not back\n";
    }

    l->pop_back();
    l->pop_front();

    if (l->front() != l->back() && l->front() != 2) {
        cout << "2 should be front and back\n";
    }

    l->pop_back();
    if (!l->empty()) {
        cout << "list not empty\n";
    }

    list<int> l2;
    l2->push_back(1);

    if (l == l2) {
        cout << "lists are different\n";
    }
    l = l2;
    if (l != l2) {
        cout << "list are the same\n";
    }

    return 0;
}