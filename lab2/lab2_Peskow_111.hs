{-1-}
{- (1)Список натуральных чисел.-}
list1 :: Int -> [Int]
list1 0 = []
list1 x = list1 (x-1) ++ [x]

{-(2)Список нечетных натуральных чисел.-}

list2 :: Int -> [Int]
list2 0 = []
list2 x = list2 (x-1) ++ [x*2-1]

{-(3)Список четных натуральных чисел.-}

list3 :: Int -> [Int]
list3 0 = []
list3 x = list3 (x-1) ++ [x*2]

{-(4)Список квадратов натуральных чисел.-}

list4 :: Int -> [Int]
list4 0 = []
list4 x = list4 (x-1) ++ [x^2]

{-(5)Список факториалов.-}

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

list5 :: Int -> [Int]
list5 0 = []
list5 x = list5 (x-1) ++ [factorial x]

{-(6) Список степеней двойки.-}

list6 :: Int -> [Int]
list6 0 = []
list6 x = list6 (x-1) ++ [2^x]

{-(7) Список треугольных чисел.-}

triangle :: Int -> Int  {-функция для вычисления треугольных чисел-} 
triangle n = if n == 1 then 1 else n + triangle (n-1)

list7 :: Int -> [Int]
list7 0 = []
list7 x = list7 (x-1) ++ [triangle x]

{-(8) Список пирамидальных чисел-}

four :: Int -> Int 
four n = if n==1 then 1 else triangle(n)+ four(n-1) {-фунция trinagle описана выше-}

list8 :: Int -> [Int]
list8 0 = []
list8 x = list8 (x-1) ++ [four x ]

{-2-}
{-(1) Функция, принимающая на входе список вещественных чисел и
вычисляющую их арифметическое среднее. Постарайтесь, чтобы
функция осуществляла только один проход по списку.-}

dlina :: [Double] -> Double --считаем количество элементов в списке
dlina [x] = 1 
dlina (x:xs) = dlina(xs) + 1 

aprxSumList :: [Double] -> Double --считаем сумму всех элементов
aprxSumList [x] = x
aprxSumList (x:xs) = aprxSumList(xs) + x

midValue :: [Double] -> Double		--сумму делим на количество
midValue []=0
midValue (x) = aprxSumList (x)/dlina (x)

{-(2) Функция вычленения n-го элемента из заданного списка.-}


find ::([Int],Int) -> Int
find ((xs),n) = if n == 1 then head (xs) else find ((tail (xs)), (n - 1)) 

{-(3) Функция сложения элементов двух списков. Возвращает список,
составленный из сумм элементов списков-параметров. Учесть,
что переданные списки могут быть разной длины.-}

dlinaInt :: [Int] -> Int --тоже что и dlina только для Int
dlinaInt [x] = 1 
dlinaInt (x:xs) = dlinaInt(xs) + 1

summirovanie :: ([Int],[Int]) -> [Int]
summirovanie ([],(ys)) = (ys)
summirovanie ((x:xs),(y:ys)) = if dlinaInt (x:xs) <= dlinaInt (y:ys) then [x+y] ++ summirovanie((xs,ys))   else [x+y] ++ summirovanie((ys,xs))

{-(4) Функция перестановки местами соседних четных и нечетных
элементов в заданном списке-}

swap :: [Int] -> [Int]
swap [x] = [x]
swap [] = []
swap (x:y:sp) = if (odd x && odd y)||(even x && even y) then [y] ++ [x] ++ swap (sp) else [x] ++ swap(y:sp)
{-если первые два четные то меня местами, иначе передвигаемся на элемент-}

{-(5) Функция twopow n, которая вычисляет 2^n, исходя из следующих соображений. Пусть необходимо возвести 2 в степень n. Если n четно, т.е. n = 2k, то 2n = 22k = (2k)2.Если n нечетно,т.е.n=2k+1,то2n=22k+1=2·(2k)2.Функция twopow не должна использовать оператор ^ или любую функцию возведения
в степень из стандартной библиотеки. Количество рекурсивных
вызовов функции должно быть пропорционально log n.-}

twopow :: Integer -> Integer
twopow 0 = 1
twopow 1 = 2
twopow 2 = 4
twopow n = if even n then twopow(div n 2) * twopow(div n 2) else 2 * twopow(div (n - 1) 2) * twopow(div (n - 1) 2)
{-если четно, то считаем произведения функции с фактическим аргументов деленны мна два-}

{-(6) Функция removeOdd, которая удаляет из заданного
списка целых чисел все нечетные числа. Например:
removeOdd [1,4,5,6,10] должен возвращать [4,10].-}

removeOdd :: [Int] -> [Int]
removeOdd [] = []
removeOdd (x:xs) = if odd x then removeOdd (xs) else [x] ++ removeOdd (xs) 

{-(7) Функция removeEmpty, которая удаляет пустые строки из заданного списка строк. Например:
removeEmpty ["", "Hello", "", "", "World!"]
возвращает ["Hello","World!"].-}

removeEmpty :: [String] -> [String]
removeEmpty [] = []
removeEmpty (x:xs) = if x=="" then removeEmpty (xs) else [x] ++ removeEmpty (xs) --аналогично заданию предыдущему

{-(8) Функция countTrue :: [Bool] -> Integer, возвращающая количество элементов списка, равных True.-}

countTrue :: [Bool] -> Integer
countTrue []= 0
countTrue (x:xs) = if x==True then 1 + countTrue (xs) else countTrue(xs) --аналогично предыдущему заданию

{-(9) Функция makePositive, которая меняет знак
всех отрицательных элементов списка чисел, например: makePositive [-1, 0, 5, -10, -20] дает
[1,0,5,10,20]-}

makePositive :: [Integer] -> [Integer]
makePositive [] = []
makePositive (x:xs) = if x > 0 then x : makePositive xs else -x : makePositive xs

{-(10) Функция delete :: Char -> String -> String, которая принимает на вход строку и символ и возвращает
строку, в которой удалены все вхождения символа. Пример: delete ’l’ "Hello world!" должно возвращать "Heo
word!".-}

delete :: Char -> String -> String
delete symb [] = []
delete symb (x:xs) = if x == symb then delete symb (xs) else [x] ++ delete symb (xs)

{-(11) Функция substitute :: Char -> Char -> String -> String,
которая заменяет в строке указанный символ на заданный. Пример: substitute ’e’ ’i’ "eigenvalue" возвращает
"iiginvalui"-}

substitute :: Char -> Char -> String -> String
substitute symb1 symb2 [] = []
substitute symb1 symb2 (x:xs) = if x == symb1 then [symb2] ++ substitute symb1 symb2 (xs) else [x] ++ substitute symb1 symb2 (xs)

