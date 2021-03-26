{-7-}
{-Определим следующий набор операций над строками:
Очестка: удаление всех символов из строки
Удаление: удаление всех вхождений указанного символа
Замена: замена всех вхождений одного символа на другой
Добавление: дабавление в начало строки указанного символа

Разработайте тип данных, характеризующий операции над строками. 
Определите следующие функции-}

data Maybe a = Nothing | Just a
data Action     = Clean 				--тип операций работы со строкой
             	| Delete Char
           	    | Swap Char Char 
                | Pluschar Char deriving (Eq, Show)

clean :: String -> String			--функция для удаления всех символов из строки
clean "" = ""
clean (x:xs) = clean(xs)

delete :: Char -> String -> String			-- функция удаление заданного наперед символа из строки
delete symb [] = []
delete symb (x:xs) = if x == symb then delete symb (xs) else [x] ++ delete symb (xs)

swap :: Char -> Char -> String -> String			-- фукнция, которая мменяет местами два заданных символа в заданой строке
swap symb1 symb2 [] = []
swap symb1 symb2 (x:xs) = if x == symb1 then [symb2] ++ swap symb1 symb2 (xs) else [x] ++ swap symb1 symb2 (xs)

pluschar :: Char -> String -> String			-- функция, которая впереди строки добавляет заданный символ
pluschar symb [] = [symb]
pluschar symb xs = [symb] ++ xs 



process :: Action -> String -> String			-- функция, которая принимает тип действия со строкой и саму строку для ее обработки
process ( Clean ) str = clean str                 -- очистка
process ( Delete symb ) str = delete symb str       -- удаление
process ( Swap symb1 symb2 ) str = swap symb1 symb2 str -- замена
process ( Pluschar symb) str = pluschar symb str       -- добавление

processAll :: [Action] -> String -> String            --функция принимает список действий и строку
processAll [] str = str      --если действий нет
processAll (x:xs) str = let s = process x str      -- сопоставляем с образом, выполняем первое действие и сохраняем результат
						in processAll xs s

deleteAll :: String -> String -> String     -- принимает две строки
deleteAll [] str2 = str2            --если ничего не надо удалятт
deleteAll str1 [] = []             --если не из чего удалять
deleteAll (x:xs) str2 = let s = processAll [Delete x] str2       -- сохраняем результат первого действия
						in deleteAll xs s           --выполняем следующее удаление





		