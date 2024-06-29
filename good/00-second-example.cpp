tuple<bool, int> find_position(list<int> &numbers, int n) {
    if (numbers -> empty()) {
        return make_tuple(false, 0);
    }
    if (numbers->front() == n) {
        return make_tuple(true, 0);
    } else {
        numbers->pop_front();
        bool exist;
        int position_suf;
        tie(exist, position_suf) = find_position(numbers, n);
        return make_tuple(exist, 1 + position_suf);
    }
}

int main() {
    int n = 42; list<int> numbers;
    for (int i = 0; i <= n; i++) {
        numbers->push_back(i);
    }

    int m; bool ok;
    tie(ok, m) = find_position(numbers, n); // numbers 0..42
    if (!(ok && m == n && numbers -> empty())) {
        cout << "tu wejdziemy\n";
    }
  
    for (int i = 0; i < 1000; i++) {
        if (i < 42) {
        continue;
        }
        cout << "wypisze tylko " << i << "\n";
        break;
    }
    return 0;
}
