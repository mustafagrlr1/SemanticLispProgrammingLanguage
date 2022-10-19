%knowledge base

flight(canakkale, erzincan, 6).
flight(erzincan, canakkale, 6).

flight(erzincan, antalya, 3).
flight(antalya, erzincan, 3).

flight(izmir, antalya, 2).
flight(antalya, izmir, 2).

flight(diyarbakir, antalya, 4).
flight(antalya, diyarbakir, 4).

flight(izmir, istanbul, 2).
flight(istanbul, izmir, 2).

flight(izmir, ankara, 6).
flight(ankara, izmir, 6).

flight(diyarbakir, ankara, 8).
flight(ankara, diyarbakir, 8).

flight(istanbul, ankara, 1).
flight(ankara, istanbul, 1).

flight(istanbul, rize, 4).
flight(rize, istanbul, 4).

flight(rize, ankara, 5).
flight(ankara, rize, 5).

flight(ankara, van, 4).
flight(van, ankara, 4).

flight(van, gaziantep, 3).
flight(gaziantep, van, 3).
%burda yapabilecegimiz en guzel sey recursion bunun icin de resolution ve unification kullanilabilir
%26.derste anlatildigi gibi
%ilk route a programin girmemesi gerekiyor ikinci loopa ise program gircek ve bidaha recursion deniyecek
%ilk predicate a girmemesi ve yanlis oldugunu programin anlamasi icin mantikli bir parametre bul
%istenilen parametreleri iyi belirle gerisi kolay

%rules

route(X,Y,C) :-
  flight(X,Y,C).

route(X, Y, T) :-
    cost(X, Y, T,[]).
%    not(X==Y),not(Z==Y),not(X==Z),
%    T is Cost1 + Cost2.

cost(X,Y,C,_) :- flight(X,Y,C).
cost(X,Y,C,Flights) :-
  not(member(X,Flights)),
  flight(X,Z,Cost1),%yakin olan yolu belirliyor
  cost(Z,Y,Cost2,[X|Flights]),%yakin yolun son kismina ekliyerek yeni yolu bulur
  not(X==Y),
  C is Cost1+Cost2.
% route(X, Y, T) :-
%     flight(X, Z, Cost1),
%     flight(Z, C, Cost2),
%     route(C, Y, Cost3),
%     not(X==Y),not(Z==Y),not(X==Z), not(C==Z), not(X==C),not(C==Y),
%     T is Cost1 + Cost2 + Cost3.
