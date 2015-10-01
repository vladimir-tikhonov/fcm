[![Build Status](https://travis-ci.org/vladimir-tikhonov/fcm.svg?branch=master)](https://travis-ci.org/vladimir-tikhonov/fcm)
# Fcm
Лабораторная # 1 по ФП.

(c) Тихонов Владимир, гр. 151003.

# Параметры
```bash
fcm FILE [-o|--output FILE] [-c|--clusters COUNT] [-m|--metric NAME]
           [-p|--precision VALUE] [-i|--initializer NAME]

Опции:
  -h,--help                Помощь
  FILE                     Файл с данными для кластеризации.
  -o,--output FILE         Файл, куда будут записаны результаты.
  -c,--clusters COUNT      Количество кластеров, по умолчанию 5.
  -m,--metric NAME         Используемая метрика (Euclid или Hamming), по
                           умолчанию Euclid.
  -p,--precision VALUE     Точность вычислений, по умолчанию 0.0001.
  -i,--initializer NAME    Метод инициализации начальных значений
                           (BelongingDegree или Centers), по умолчанию
                           BelongingDegree.
```
# Запуск с помощью sandbox
```bash
git clone https://github.com/vladimir-tikhonov/fcm.git
cd fcm
cabal sandbox init
cabal install --only-dependencies
cabal build
./dist/build/fcm/fcm samples/butterfly.txt -o result.txt -c 10 -m Hamming -p 0.0001 -i Centers
```
