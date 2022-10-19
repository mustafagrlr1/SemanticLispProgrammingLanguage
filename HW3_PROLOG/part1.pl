
:-dynamic(room/3).
:-dynamic(course/7).
:-dynamic(instructor/3).
:-dynamic(student/3).
:-dynamic(occupancies/4).

room(z23, 200, [projector, smartboard]).
room(z10, 50, [projector, smartboard]).
room(z11, 49, [projector, smartboard]).
room(z5, 70, [smartboard]).
%
% %kurs id, kurs instructor, kurs capacity, course needs, special student enrolled
course(cse101, gokturk, 100, 3 , z23, [projector, smartboard], n).
course(cse102, genc, 105, 3, z23, [projector], y).
course(cse484, akgul, 45, 4, z10, [smartboard], y).
course(cse341, genc, 240, 5, z10 , [], n).
course(cse222, sevilgen, 104, 6, z11 , [projector, smartboard], n).
course(cse241, akgul, 180,1, z5 ,[], n).
% %
instructor(gokturk, [cse101],[smartboard]).
instructor(genc, [cse102, cse341], []).
instructor(akgul, [cse241, cse484], [smartboard, projector]).
instructor(sevilgen, [cse222], [projector]).



% %id soyisim, ozel durum
student(171044034, [cse101, cse102], n).
student(171044062, [cse101, cse102, cse484], n).
student(171044069, [cse341, cse102, cse222, cse241], n).
student(181022030, [cse102, cse484, cse222], n).
student(190200210, [cse101, cse102, cse341, cse222], y).
student(181042103, [cse101, cse102, cse341], n).

occupancies(z23, 8, cse101).
occupancies(z23, 9, cse101).
occupancies(z23, 10, cse102).
occupancies(z23, 11, cse102).
occupancies(z23, 14, cse101).
occupancies(z23, 15, cse101).
occupancies(z10, 8, cse484).
occupancies(z10, 9, cse484).
occupancies(z10, 11, cse341).
occupancies(z11, 8, cse222).
occupancies(z11, 11, cse222).
occupancies(z11, 12, cse222).
occupancies(z5, 8, cse241).
occupancies(z5, 9, cse241).
occupancies(z5, 13, cse241).
occupancies(z5, 14, cse241).


% conflict(CourseID, CourseID2):-
%   course


%student ekleyebilmek icin id sini kurslarini ve ozel bir ogrenci olmasini arguman olarak aliyoruz
add_student(ID, Courses, SC) :-
  not(student(ID,_,_)),
  check_course(Courses),
  assert(student(ID, Courses, SC)).

%
%course ekleyebilmek icin id farkli olmali
add_course(ID, Instructor, Capacity, Hour, Room, Needs, Hd) :-
  not(course(ID,_,_,_,_,_,_)),
  instructor(Instructor,_,_),
  assert(course(ID, Instructor, Capacity, Hour, Room, Needs, Hd)).
%
% %room ekleyebilmek icin id farkli olmali
 add_room(ID, Capacity,RN) :-
   not(room(ID,_,_)),
   assert(room(ID,Capacity,RN)).

check_course([]).
check_course([H|T]):-
  course(H,_,_,_,_,_,_),
  check_course(T).


%Room id ve course id
assign(RID, CID) :-
  course(CID,_,Capacity, _, _, Needs, _),
  room(R,CapacityRoom, NeedsRoom),
  CapacityRoom >= Capacity, %roomun kapasitesi buyuk olmasi gerekiyor dersinkinden
  listControl(Needs, NeedsRoom),% listenin course un ihtiyaclarini karsiliyor mu diye bakiyor
  RID = R.
%  occupancyControl(8,0,R,Hour),
listControl([],_).
listControl([H|T],List):-
  member(H, List),
  listControl(T, List).



enroll(SID, CID):-
  student(SID, _, _),
  course(CID, _,Ca, _, RID, _,_),
  room(RID, _, _),
  course_student_count(CID, Count),
  (Count >= Ca).

conflict(CID1,CID2):-
    course_hour(8,CID1,[],C1),
    course_hour(8,CID2,[],C2),
    \+compare_lists(C1,C2).

compare_lists([],_).
compare_lists([H|T],List):-
    not(member(H,List)),
    compare_lists(T,List).
course_hour(18,CID,Count,Count):- course(CID,_,_,X,_,_,_),length(Count,Len),Len == X.
course_hour(Hour,CD1,Count,C):-
    Hour < 18,
    occupancies(_,Hour,CD1),
    Hour1 is Hour + 1,
    append(Count,[Hour],Res),
    course_hour(Hour1,CD1,Res,C).

course_hour(Hour,CID,Count,C):-
    Hour < 18,
    Hour1 is Hour + 1,
    course_hour(Hour1,CID,Count,C).
% which_class_assigned(SID):-
%   student(SID,Y,_),
%   forall(course(X,_,_,_,_,_,_), print_others(X,Y)).
%
%
% print_others([],_).
% print_others(H,List):-
%   not(member(H,List)),writeln(H).

course_student_count(CID,Count):-
  student(CID, X, _),
  course_student_count(CID,Count,X).
course_student_count(CID,Count,Liste):-
  member(CID,Liste),
  Count is Count + 1.
% occupancyControl(Start, Count, Room, Hour):-
%   18 < Start,
%   Start is Start + 1,
%   Count == Hour;
%   (Count is Count + 1, occupancyControl(Start, Count, Room, Hour)).
