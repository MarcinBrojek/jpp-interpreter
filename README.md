#### JĘZYK

Język imperatywny, w składni opartej o podzbiór języków C/C++. Jest on lekko zmodyfikowany, tak by mógł spełniać założone funkcjonalności. Program składa się z deklaracji zmiennych/funkcji. Uruchomniony program wykona zadeklarowaną funkcję main. Wiecej informacji o składni w deklaracji języka: Marcin_Brojek.md.

Dostarczony interpreter spełnia zawarte założenia z deklaracji za wyjątkiem niepoprawnej metody get dla tupli.

#### OGÓLNY OPIS ROZWIĄZANIA

Rozwiązanie składa się dwóch głównych modułów: **TypeChecker** i **Interpreter**, oba korzystają jednocześnie z monad: Reader, State, Except. W przypadku TypeChecker'a w środowisku i stanie są przechowywane informacje dotyczące typów, natomiast w Interpreter'ze są to wartości.

W obrębie bloku - ciągu instrukcji mogą pojawiać się w dowolnych momentach deklaracje zmiennych, funkcji jak i procedur, dlatego oba moduły powyżej aplikują kolejne instrukcje do tworzonych na nowo, kolejnych środowisk.

#### URUCHAMIANIE

Kompilacja przy użyciu polecenia **make** utworzy katalog **build** i plik wykonywalny **interpreter**, który można uruchamiać poleceniem **./interpreter program**.

#### PRZYKŁADY

W katalogu **bad**, pliki są postaci **XX-YY-name-of-test.cpp**, gdzie **XX** równe **00** oznacza błąd w składni, **10** błędy wykonania - wychwycone dynamicznie, natomiast **12** błędy podczas statycznego typowania. 

Nazwy w katalogu **good** ograniczają tylko do **YY-name-of-test.cpp** i również tutaj **YY** to numer funkcjonalności z listy Język Imperatywny. Za wyjątkiem **99**, które wskazują na inne, rozbudowane przykłady programów.

Aby uruchomić wszystkie przykłady w zależności od katalogu, można użyć polecenia: **make good_run** lub **make bad_run**.