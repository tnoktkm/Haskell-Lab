import System.IO
import System.Environment
import Data.List
{-1.  Напишите следующие программы:
(1) Программа, считывающая два числа и возвращающая их сумму.-}

sumAB :: IO Int   --функция вычисляющая сумма двух чисел.
sumAB = do  a <- getLine   --ввод первого числа на первой строке
            b <- getLine   --ввод второго числа на второй строке
            return (read a + read b) --read - возвращает число от заданной строки, следовательно
            --преобразуем наши строки и складываем их и в итоге выводим.

{-(2) Программа, распечатывающая переданные в нее аргументы командной строки.
-}

stroka :: [String] -> IO ()         --функция выводящая последовательно элементы списка типа строка (пробелом между ними)
stroka [] = do putStr "\n"			--если пустой список выводим перевод строки (пустой список в конце)	
stroka (x:xs)= do putStr x   	 	--если не пустой список, выводим первый элемент, пробел, и вызываем туже только без первого элемента функцию
                  putStr " "
                  stroka xs   



main :: IO ()		--функция считывает, но не возвращает ничего
main = do x <- getArgs		--считывает список аргументов
          stroka x          --вызывает функцию stroka
{-Для проверки либо собрать exe-шник и запускать из
 консоли с параметрами,
 либо воспользоваться командой :main-}

{-(3) Программа, которая принимает в командной строке имя файла
и распечатывает его на экране.-}

printFile :: IO String
printFile = do fileName <- getLine  --вводим полный путь файла
               fromHandle <- openFile fileName ReadMode --открываем файл
               text <- hGetContents fromHandle -- сохраняем текст файла
               return (text) -- выводим текст


{-(4) Программа, принимающая в командной строке число n и
имя файла и выводящая на экран первые n строк файла (используйте функцию lines, разбивающую строку
на список строк в символах конца строки, т.е., например lines"line1\nline2" вернет ["line1", "line2"].
Также полезна функция unlines, осуществляющая обратную
операцию.)-}

firstNStr :: Int -> [String] -> [String]--функция возвращающая n элементов списка строк 
firstNStr 0 (x:xs) = []
firstNStr _ []     = []
firstNStr n (x:xs) 
			   | n <= length (x:xs)  = x : firstNStr (n - 1) xs--рекурсия в функции
               | otherwise = error "arg must be <= string length"--ошибка в случае если колличество требуемых сторк больше чем строк в списке

printFileNString :: IO String   --функция выводит первые amountStr строк файла
printFileNString = do fileName <- getLine --считываем имя файла для работы
                      amountStr <- getLine    --считываем количество строк для вывода
                      fromHandle <- openFile fileName ReadMode   --открытие файла для чтения
                      text <- hGetContents fromHandle   --строка всего файла
                      return (unlines (firstNStr (read amountStr) (lines text))) --выводим результат работы


{-(2).  Реализуйте программы, выполняющие задания вашего
варианта из первой  лабораторной  работы. Параметры
функций  должны  считываться с клавиатуры.-}

{-(1) Функция max3, по трем целым возвращающая наибольшее из них.-}

max3 :: IO Int
max3 = do x <- getLine 
          y <- getLine  --считываем
          z <- getLine
          return (max (read x) (max (read y) (read z)))

{-(2) Функция min3, по трем целым возвращающая наименьшее из них.-}

min3 :: IO Int
min3 = do x <- getLine
          y <- getLine
          z <- getLine
          return (min (read x) (min (read y) (read z)))

{-(3) Функция sort2, по двум целым возвращающая пару,
в которой наименьшее из них стоит на первом месте,
а наибольшее — на втором.-}

sort2 :: IO (Integer, Integer)
sort2 =  do x <- getLine -- считываем данные
            y <- getLine
            if x < y then return ((read x), (read y)) 
                     else return ((read y), (read x))

{-(4) Функция bothTrue :: Bool -> Bool -> Bool, которая возвращает True тогда и только тогда, когда оба ее аргумента будут равны True. Не используйте при определении функции стандартные логический операции (&&, || и т.п.).-}

bothTrue :: IO Bool
bothTrue = do a <- getLine--ввод данных True или False ввиде строки
              b <- getLine
              if (read a) == True  then if (read b) == True--проверка осуществляется с помощью сравнения строк
			                               then return True 
			                            else return False 
			  else return False

{-(5) Функция solve2::Double->Double->(Bool,Double),
которая по двум числам, представляющим собой коэффициенты
линейного уравнения ax + b = 0, возвращает пару, первый
элемент которой равен True, если решение существует и
False в противном случае; при этом второй элемент равен
 либо значению корня, либо 0.0.-}

solve2:: IO (Bool, Double)
solve2 = do a <- getLine  
            b <- getLine 
            if (read a) /= 0.0 && (read b) /= 0.0 
               then return (True, (-(read b)) / (read a)) 
            else return (False, 0.0)

{-(6) Функция isParallel, возвращающая True,
если два отрезка, концы которых задаются в
аргументах функции, параллельны (или лежат
на одной прямой). Например, значение 
выражения isParallel (1,1) (2,2) (2,0) (4,2) должно 
быть равно True, поскольку отрезки (1, 1) − (2, 2) и (2, 0) − (4, 2) 
параллельны.-}

isParallel :: IO Bool
isParallel = do x1 <- getLine--считьваем координаты
                y1 <- getLine--
                x2 <- getLine--
                y2 <- getLine--
                x3 <- getLine--
                y3 <- getLine--
                x4 <- getLine--
                y4 <- getLine--
                if ((read x1) - (read x2)) / ((read y1) - (read y2)) == ((read x3) - (read x4)) / ((read y3) - (read y4)) 
                   then return True 
                else return False

{-(7) Функция isIncluded, аргументами которой служат 
параметры двух окружностей на плоскости (координаты центров 
и радиусы); функция возвращает True, если вторая окружность 
целиком содержится внутри первой.-}

isIncluded :: IO Bool
isIncluded  = do x1 <- getLine--считываем координаты центра окружности1
                 y1 <- getLine
                 r1 <- getLine--радиус 1
                 x2 <- getLine--считываем координаты центра окружности1
                 y2 <- getLine
                 r2 <- getLine--радиус 1
                 if (((((read x1) - (read x2))^2) + (((read y1) - (read y2))^2))^1 / 2) + (read r2) < (read r1)--растояние между центрами окружностей сложенное с радиусом второй окружности должно быть меньше радииуса первой 
                 then return True 
                 else return False

{-(8) Функция isRectangular, принимающая в 
качестве параметров координаты трех точек на 
плоскости, и возвращающая True, если образуемый 
ими треугольник — прямоугольный.-}

isRectangular :: IO Bool
isRectangular = do x1 <- getLine --считываем первую точку
                   y1 <- getLine
                   x2 <- getLine---считываем вторую точку
                   y2 <- getLine
                   x3 <- getLine----считываем третью точку
                   y3 <- getLine
                   if ((((read x1) - (read x2))^2) + (((read y1) - (read y2))^2)) + ((((read x1) - (read x3))^2) + (((read y1) - (read y3))^2)) == ((((read x2) - (read x3))^2) + (((read y2) - (read y3))^2)) || ((((read x1) - (read x2))^2) + (((read y1) - (read y2))^2)) + ((((read x2) - (read x3))^2) + (((read y2) - (read y3))^2)) == ((((read x1) - (read x3))^2) + (((read y1) - (read y3))^2)) || ((((read x1) - (read x3))^2) + (((read y1) - (read y3))^2)) + ((((read x2) - (read x3))^2) + (((read y2) - (read y3))^2)) == ((((read x1) - (read x2))^2) + (((read y1) - (read y2))^2)) 
                   -- стандартная проверка по т. Пифагора.
                      then return True 
                   else return False

{-(9) Функция isTriangle, определяющая, 
можно ли их отрезков с заданными длинами x, y и z 
построить треугольник.-}

isTriangle :: IO String
isTriangle = do x <- getLine  --считываем х
                y <- getLine  --считываем у
                z <- getLine  --считываем z
                if (read x) + (read y) > (read z) && (read x) + (read z) > (read y) && (read y) + (read z) > (read x) 
                   then return "Yes" 
                else return "No" 

{-(10) Функция isSorted, принимающая на вход три 
числа и возвращающая True, если они упорядочены 
по возрастанию или по убыванию.-}

check :: Integer -> Integer -> Integer -> Bool--функция для цисел на упорядоченность
check x y z = if (x < y && y < z) || (x > y && y > z) 
				 then True 
		      else False

isSorted :: IO Bool
isSorted = do x <- getLine  --считываем х
              y <- getLine  --считываем у
              z <- getLine   --считываем z
              if check (read x) (read y) (read z)
                 then return True 
              else return False