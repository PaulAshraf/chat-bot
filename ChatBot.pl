
totalCal(1800).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

altList([],[],[]).
altList([],Y,Y).
altList(X,[],X).
altList([H|T],[Y|Z],[H,Y|K]):-
                         altList(T,Z,K), !.
                         
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

readInputTillQuit :-  writeln("Welcome to your personal assistant") ,
                      write('> '),
                      res(X),
                      readInputTillQuit_helper(X,[],[]).



readInputTillQuit_helper(X,PQ,PR) :-  X = [quit,'.'],
                                      altList(PQ,PR,HL),
                                      foodFromHistoryWithMeal(HL,FL),
                                      write('> '),
                                      printReportBreakfast(FL),
                                      printReportLunch(FL),
                                      printReportDinner(FL),
                                      write("Bye").

                      
readInputTillQuit_helper(X,PQ,PR) :-
                                   X \= [quit,'.'],
                                   response(X,PQ,PR,R),
                                   ws(R),
                                   nl,
                                   write('> '),
                                   append([X],PQ,PQ1),
                                   append([R],PR,PR1),
                                   res(X1),
                                   readInputTillQuit_helper(X1,PQ1,PR1).
                                   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
printReportBreakfast([]):- writeln("You had - for breakfast").


printReportBreakfast([FLH|FLT]):- FLH = [T,breakfast] ,
                      write("You had "),
                      write(T),
                      writeln(" for breakfast").

printReportBreakfast([FLH|FLT]):- FLH \= [T,breakfast] ,
                                  printReportBreakfast(FLT).
                      
                      
printReportLunch([FLH|FLT]):- FLH = [T,lunch] ,
                      write("You had "),
                      write(T),
                      writeln(" for lunch").

printReportLunch([FLH|FLT]):- FLH \= [T,lunch] ,
                      printReportLunch(FLT).
                      
printReportLunch([]):- writeln("You had - for lunch").
                      
printReportDinner([FLH|FLT]):- FLH = [T,dinner] ,
                      write("You had "),
                      write(T),
                      writeln(" for dinner").

printReportDinner([FLH|FLT]):- FLH \= [T,dinner] ,
                      printReportDinner(FLT).

printReportDinner([]):-   writeln("You had - for dinner").
                      
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%DCG%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%a
valid(Q) :- Q =   ['how','many','calories','does',T,'contain'].
           % (  prop(T,contain,_) ; prop(I,is,_) ) .

            

%b
 valid(Q) :- Q =   ['what','does',T,'contain'].
           %  prop(T,contain,_) .

 %c
 valid(Q) :- Q = ['can','i','have',T,'for',M].
          %  prop(T,contain,_), prop(_,not,M).

 %d
 valid(Q) :- Q = ['what', 'is',I].
         %  prop(I,is,_).

 %e
 valid(Q) :- Q = ['how', 'many', 'calories', 'do', 'i', 'have', 'left'].

 %f
 valid(Q) :- Q =['what', 'kind', 'of', C, 'does', T, 'contain'].
          % prop(T,contain,_) , prop(_,is,C) .

%g
valid(Q) :- Q = ['is',I, 'a' ,C, 'in', T, '?']  .
         % prop(T,contain,_) , prop(_,is,C),  prop(I,is,_).

%h
valid(Q) :- Q = ['what', 'can', 'i', 'have', 'for', M,'that', 'contains', I].
         %  prop(_,not,M) , prop(I,is,_).

 %a
valid(Q) :- Q = ['i', 'ate', T ,'for', M].
           %  prop(_,not,M) ,  prop(T,contain,_).

%b
valid(Q) :- Q = ['i', 'do', 'not', 'eat', I].
        %  prop(I,'is',_).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



isValid(Q) :- valid(Q).


%filterProp(R,[]).
filterProp(R,CL):- bagof((X,Y),prop(X,R,Y),CL).
                         
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

matchFirst(T1,[],[]):- !.
matchFirst(T1,[H|T],[X|Y]) :- length([H|T],R),
                        length([X|Y],R),
                        H = (P1,P2),
                        T1 = P1,
                        X = E-Occ,
                        P2 = E,
                        Occ = 1,
                        matchFirst(T1,T,Y).
                        
matchFirst(T1,[],[]):- !.
matchFirst(T1,[H|T],[X|Y]) :- length([H|T],R),
                        length([X|Y],R),
                        H = (P1,P2),
                        T1 = P1,
                        X = E-Occ,
                        P2 \= E,
                        Occ = 0,
                        matchFirst(T1,T,Y).
                        
matchFirst(T1,[],[]):- !.
matchFirst(T1,[H|T],[X|Y]) :- length([H|T],R),
                        length([X|Y],R),
                        H = (P1,P2),
                        T1 \= P1,
                        X = E-Occ,
                        P2 = E,
                        Occ = 0,
                        matchFirst(T1,T,Y).
                        
matchFirst(T1,[],[]):- !.
matchFirst(T1,[H|T],[X|Y]) :- length([H|T],R),
                        length([X|Y],R),
                        H = (P1,P2),
                        T1 \= P1,
                        X = E-Occ,
                        P2 \= E,
                        Occ = 0,
                        matchFirst(T1,T,Y).
                        
                        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                        

matchSecond(T1,[],[]):- !.
matchSecond(T1,[H|T],[X|Y]) :- length([H|T],R),
                        length([X|Y],R),
                        H = (P1,P2),
                        X = E-Occ,
                        T1 = P2,
                        P1 = E,
                        Occ = 1,
                        matchSecond(T1,T,Y).

matchSecond(T1,[],[]):- !.
matchSecond(T1,[H|T],[X|Y]) :- length([H|T],R),
                        length([X|Y],R),
                        H = (P1,P2),
                        X = E-Occ,
                        T1 \= P2,
                        P1 = E,
                        Occ = 0,
                        matchSecond(T1,T,Y).

matchSecond(T1,[],[]):- !.
matchSecond(T1,[H|T],[X|Y]) :- length([H|T],R),
                        length([X|Y],R),
                        H = (P1,P2),
                        X = E-Occ,
                        T1 = P2,
                        P1 \= E,
                        Occ = 0,
                        matchSecond(T1,T,Y).

matchSecond(T1,[],[]):- !.
matchSecond(T1,[H|T],[X|Y]) :- length([H|T],R),
                        length([X|Y],R),
                        H = (P1,P2),
                        X = E-Occ,
                        T1 \= P2,
                        P1 \= E,
                        Occ = 0,
                        matchSecond(T1,T,Y).

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 % count([],C,C,A).
 % count([H|T],C,Z,A) :-
 %              H = E-Occ,
  %             E = A,
   %            C1 is C+Occ,
    %           count(T,C1,Z,A).

%  count([H|T],C,Z,A) :-
 %              H = E-Occ,
  %             E \= A,
   %            C1 is C+0,
    %           count(T,C1,Z,A).

%mergeMatchLists(ML1,ML2,[U|V]) :- append(ML1,[H|T],ML2),
%                              H = E-Occ1,
%                             count([H|T],0,Z,E),
%                               U = E-Z,
%                               mergeMatchLists()

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 deleteE(E,L,R):- deleteE_helper(E,L,R,[]).

deleteE_helper(E,[],R,R).

deleteE_helper(E,[H|T],R,Acc):- H = E1-Occ,
                                E = E1,
                                deleteE_helper(E,T,R,Acc).


deleteE_helper(E,[H|T],R,Acc):- H = E1-Occ,
                                E \= E1,
                                append(Acc,[H],AccRes),
                                deleteE_helper(E,T,R,AccRes).
                                
                                

countOcc([H|T],E,R) :- countOcc_helper([H|T],E,R,0).


countOcc_helper([],E1,R,R).

countOcc_helper([H|T],E1,R,Acc) :-  H = E-Occ,
                         E1 = E,
                         AccRes is Acc + Occ,
                         countOcc_helper(T,E1,R,AccRes).
                         
countOcc_helper([H|T],E1,R,Acc) :- H = E-Occ,
                         E1 \= E,
                         AccRes is Acc + 0,
                         countOcc_helper(T,E1,R,AccRes).
                         
mergeMatchLists(ML1,ML2,R) :- append(ML1,ML2,L),
                                mergeMatchListsAppend(L,R).

mergeMatchListsAppend(L,R):- mergeMatchLists_helper(L,R,[]).
                                
 mergeMatchLists_helper([],R,R).
  
 mergeMatchLists_helper(L,R,Acc):-
                                L = [H|T],
                                H = E-Occ,
                                countOcc(L,E,Res),
                                deleteE(E,L,L1) ,
                                X = E-Res,
                                append(Acc,[X],Result),
                                mergeMatchLists_helper(L1,R,Result).
                                
                                
                                
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 

 
 bestMatches([H|T],BL) :-   H = E-Occ,
                           bestMatches_helper(T,[H],[E],BL).
 
  bestMatches_helper([],_,L,L).
 bestMatches_helper([H|T],Acc,Acc1,L):- H = E-Occ,
                                   Acc = [H1|T1],
                                   H1 = E1-Occ1 ,
                                   Occ > Occ1,
                                   bestMatches_helper(T,[H],[E],L) .
                                   
bestMatches_helper([H|T],Acc,Acc1,L):-   H = E-Occ,
                                    Acc = [H1|T1],
                                   H1 = E1-Occ1 ,
                                   Occ < Occ1,
                                    bestMatches_helper(T,Acc,Acc1,L).
                                    
                                    
 bestMatches_helper([H|T],Acc,Acc1,L):- H = E-Occ,
                                   Acc = [H1|T1],
                                   H1 = E1-Occ1 ,
                                   Occ = Occ1,
                                   append(Acc,[H],Res)  ,
                                   append(Acc1,[E],Res1) ,
                                    bestMatches_helper(T,Res,Res1,L).


                                 %%%%%%%%%
                                 
 bestMatchesOcc([H|T],BL) :-   H = E-Occ,
                           bestMatchesOcc_helper(T,[H],BL).

  bestMatchesOcc_helper([],L,L).
 bestMatchesOcc_helper([H|T],Acc,L):- H = E-Occ,
                                   Acc = [H1|T1],
                                   H1 = E1-Occ1 ,
                                   Occ > Occ1,
                                   bestMatchesOcc_helper(T,[H],L) .

bestMatchesOcc_helper([H|T],Acc,L):-   H = E-Occ,
                                    Acc = [H1|T1],
                                   H1 = E1-Occ1 ,
                                   Occ < Occ1,
                                    bestMatchesOcc_helper(T,Acc,L).


 bestMatchesOcc_helper([H|T],Acc,L):- H = E-Occ,
                                   Acc = [H1|T1],
                                   H1 = E1-Occ1 ,
                                   Occ = Occ1,
                                   append(Acc,[H],Res)  ,
                                    bestMatchesOcc_helper(T,Res,L).

 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
 
 bestMatchesMin([H|T],Min,BL) :-   H = E-Occ,
                                Occ >= Min,
                           bestMatchesMin_helper(T,Min,[E],BL).
                           
bestMatchesMin([H|T],Min,BL) :-   H = E-Occ,
                                Occ < Min,
                           bestMatchesMin_helper(T,Min,[],BL).

  bestMatchesMin_helper([],Min,L,L).
  
 bestMatchesMin_helper([H|T],Min,Acc,L):- H = E-Occ,
                                   Occ >= Min,
                                   append(Acc,[E],Res) ,
                                   bestMatchesMin_helper(T,Min,Res,L) .

bestMatchesMin_helper([H|T],Min,Acc,L):-   H = E-Occ,
                                   Occ < Min,
                                    bestMatchesMin_helper(T,Min,Acc,L).

                                    
                                    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

foodCal(F,C):-
              prop(F,contain,C,cal).
              
              
foodCal(F,C):-
              %prop(F,contain,_),
              bagof(X,prop(F,contain,X),L),
              foodCalList(L,C).
              
              


foodCalList(L,R):-   foodCalList_helper(L,0,R).
foodCalList_helper([],C,C).
foodCalList_helper([H|T],C,Z):-
                      foodCal(H,C1),
                      C2 is C+C1,
                      
                      foodCalList_helper(T,C2,Z).
                      
                      
                      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calcCalories(F,PQ,PR,C) :- calcCalories_helper(F,PQ,PR,C,0).

calcCalories_helper(0,[],[],C,Acc) :- totalCal(X),
                          C is X - Acc.

calcCalories_helper(F,[],[],C,Acc) :- totalCal(X),
                           foodCal(F,Y),  !,
                           Acc1 is Acc + Y,
                           C is X - Acc1.

calcCalories_helper(F,[PQH|PQT],[PRH|PRT],C,Acc) :-
                                        ((PQH = ['i', 'ate', T ,'for', _,'.'] , PRH = ["Ok"]) ; (PQH =  ['can', 'i', have, T, for, _ , '?'] , PRH = ['You','can','have',T,'for',_])),
                                        foodCal(T,Y), !,
                                        Acc1 is Acc + Y,
                                        calcCalories_helper(F,PQT,PRT,C,Acc1).
                                        
                                        
calcCalories_helper(F,[PQH|PQT],[PRH|PRT],C,Acc) :-
                                       ((PQH \= ['i', 'ate', T ,'for', _,'.'] , PRH \= ["Ok"]) ; (PQH =  ['can', 'i', have, T, for, _ , '?'] , PRH \= ['You','can','have',T,'for',_]) ; (PQH \=  ['can', 'i', have, T, for, _ , '?']) ),
                                        calcCalories_helper(F,PQT,PRT,C,Acc), !.
                        

  
%getIngredients(F,L):- bagof(X,prop(F,contain,X),L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



getDiffAnswer(_,[],[],[H|T],H).

getDiffAnswer(_,[],[],[],['I', told, you, that, before]).

getDiffAnswer(_,PQ,PR,[],['I', told, you, that, before]):- PQ \= [] ,
                                                                PR \= [].

getDiffAnswer(Q,[QH|QT],[RH|RT],[CH|CT],R):-  Q \= [how,many,calories,do,i,have,left,'?'],
                                              Q = QH,
                                              getDiffAnswer(Q,QT,RT,CT,R).
                                              

getDiffAnswer(Q,[QH|QT],[RH|RT],[CH|CT],R):-  Q \= QH,
                                             getDiffAnswer(Q,QT,RT,[CH|CT],R).
                                             

getDiffAnswer(Q,[QH|QT],[RH|RT],[CH|CT],R):- Q = [how,many,calories,do,i,have,left,'?'],
                                             Q = QH, CH \= RH,
                                             getDiffAnswer(Q,QT,RT,[CH|CT],R).
                                             

getDiffAnswer(Q,[QH|QT],[RH|RT],[CH|CT],R):- Q = [how,many,calories,do,i,have,left,'?'],
                                             Q = QH, CH = RH,
                                             getDiffAnswer(Q,QT,RT,CT,R).
                                             
                                             
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
responseO(Q,PQ,PR,LR):- Q = [what,can,i,have,for,M,that,contains,I,'?'],
findall(X0-0,prop(X0,contain,_),L0),
bagof(X1-1,prop(X1,contain,I),L1),
findall(X2-1,(prop(X2,contain,_),isMeal(X2,M)),L2_d),
sort(L2_d,L2),
getUnlikedIngredients(PQ,FL),
findall(X3-1,((prop(X3,contain,_),isLiked(X3,FL))),L3_d),
sort(L3_d,L3),
findall(X4-1,((prop(X4,contain,_),isBelowLimit(X4,PQ,PR))),L4_d),
sort(L4_d,L4),
mergeMatchLists(L0,L1,Res1),
mergeMatchLists(L2,Res1,Res2),
mergeMatchLists(L3,Res2,Res3),
mergeMatchLists(L4,Res3,Res4),
listOrderDesc(Res4,LR).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                         %%   RESPONSE   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% discontiguous(response/4) .

%%% QUESTION A %%% PERFECTOO %%%%

response(Q,PQ,PR,R):-  Q =  ['how','many','calories','does',T,'contain' ,'?'],
( prop(T,contain,_) ;  prop(T,is,_) ), !,
foodCal(T,C),
CR = [[C,'Calories']],
getDiffAnswer(Q,PQ,PR,CR,R).


response(Q,PQ,PR,R):-  Q =  ['how','many','calories','does',T,'contain','?'],
not( ( prop(T,contain,_) ; prop(T,is,_) ) ),
CR = [['I', do, not, know]],
getDiffAnswer(Q,PQ,PR,CR,R).



%%% QUESTION B %%%%  PERFECTOOO %%%

response(Q,PQ,PR,R):- Q =  ['what','does',T,'contain','?'],
%prop(T,contain,_),
bagof([X],prop(T,contain,X),CR),
getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,R):- Q =  ['what','does',T,'contain' ,'?'],
not(prop(T,contain,_)),
CR = [['I', do, not, know]] ,
getDiffAnswer(Q,PQ,PR,CR,R).


%%% QUESTION C %%%%  PERFECT ?? %%%%


response(Q,PQ,PR,R):- Q = ['can','i','have',T,'for',M,'?'],
 prop(T,not,M1),
 M1 \= M,
calcCalories(T,PQ,PR,C),
C >= 0,
CR = [['You','can','have',T,'for',M]],
getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,R):- Q = ['can','i','have',T,'for',M,'?'],
 not(prop(T,not,_)),
calcCalories(T,PQ,PR,C),
C >= 0,
CR = [['You','can','have',T,'for',M]],
getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,R):- Q = ['can','i','have',T,'for',M,'?'],
prop(T,not,M),
CR = [[T,'is','not','suitable','for',M]],
getDiffAnswer(Q,PQ,PR,CR,R).



response(Q,PQ,PR,R):- Q = ['can','i','have',T,'for',M,'?'],
  not(prop(T,not,_)),
calcCalories(T,PQ,PR,C),
C < 0,
CR = [['No']],
getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,R):- Q = ['can','i','have',T,'for',M,'?'],      %%%% new fix
 prop(T,not,M1),
 M1 \= M,
calcCalories(T,PQ,PR,C),
C < 0,
CR = [['No']],
getDiffAnswer(Q,PQ,PR,CR,R).



response(Q,PQ,PR,R):- Q = ['can','i','have',T,'for',M,'?'],
not(prop(T,contain,_)),
CR = [['I', do, not, know]] ,
getDiffAnswer(Q,PQ,PR,CR,R).

%%% QUESTION D %%%%  PERRFECTOO

response(Q,PQ,PR,R):-  Q = ['what', 'is',I,'?'],
prop(I,is,X),
CR = [[X]],
getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,R):-  Q = ['what', 'is',I,'?'],
not(prop(I,is,_)),
CR = [['I', do, not, know]],
getDiffAnswer(Q,PQ,PR,CR,R).

%%% QUESTION E %%%%

isType([]).

isType([PQH|PQT]):-
(PQH = ['i', 'ate', T ,'for', _,'.'] ; PQH =  ['can', 'i', have, T, for, _ , '?']),
bagof(X,prop(T,contain,X),_),
isType(PQT).

isType([PQH|PQT]):-
(PQH \= ['i', 'ate', T ,'for', _,'.'] , PQH \=  ['can', 'i', have, T, for, _ , '?']),
isType(PQT).


response(Q,[],[],R):- Q = ['how', 'many', 'calories', 'do', 'i', 'have', 'left','?'],
totalCal(C),
CR = [[C,'Calories']],
getDiffAnswer(Q,PQ,PR,CR,R), !.

response(Q,PQ,PR,R):- Q = ['how', 'many', 'calories', 'do', 'i', 'have', 'left','?'],
isType(PQ),
calcCalories(0,PQ,PR,C),
CR = [[C,'Calories']],
getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,R):- Q = ['how', 'many', 'calories', 'do', 'i', 'have', 'left', '?'],
not(isType(PQ)),
CR = [['I', do, not, know]],
getDiffAnswer(Q,PQ,PR,CR,R).

%%%% QUESTION F %%%%   % perfecto

response(Q,PQ,PR,R):- Q =['what', 'kind', 'of', C, 'does', T, 'contain','?'],
%prop(T,contain,_) , prop(_,is,C),
bagof([X],(prop(T,contain,X),prop(X,is,C)),CR),
getDiffAnswer(Q,PQ,PR,CR,R).


response(Q,PQ,PR,R):- Q =['what', 'kind', 'of', C, 'does', T, 'contain','?'],
not( bagof(X,(prop(T,contain,X),prop(X,is,C)),CR)  ),
prop(T,contain,_),  prop(_,is,C),
CR = [['Nothing', from, what, 'I', know]], !,
getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,R):- Q =['what', 'kind', 'of', C, 'does', T, 'contain','?'],
(not(prop(T,contain,_)) ; not(prop(_,is,C))),
CR = [['I', do, not, know]],
getDiffAnswer(Q,PQ,PR,CR,R).

%%%% QUESTION G %%%%   %perfecto

response(Q,PQ,PR,R):- Q = ['is',I, 'a' ,C, 'in', T, '?']  ,
prop(T,contain,I) , prop(I,is,C),
CR = [['Yes']],
getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,R):- Q = ['is',I, 'a' ,C, 'in', T, '?']  ,
(prop(I,is,_) , prop(T,contain,_)),
((not(prop(T,contain,I))) ; (not(prop(I,is,C)))),
CR = [['No']],
getDiffAnswer(Q,PQ,PR,CR,R).


response(Q,PQ,PR,R):- Q = ['is',I, 'a' ,C, 'in', T, '?']  ,
((not(prop(T,contain,_))) ; (not(prop(I,is,_)))),
CR = [['I', do, not, know]],
getDiffAnswer(Q,PQ,PR,CR,R).

%%%%% QUESTION H (sadekna) %%%%

isMeal(T,M):- not(prop(T,not,M)).

isLiked(T,[]).
isLiked(T,[FLH|FLT]):- bagof(X,prop(T,contain,X),IL),
not(member(FLH,IL)),
isLiked(T,FLT).

isBelowLimit(T,PQ,PR):- calcCalories(T,PQ,PR,C),
                        C >= 0.


response(Q,PQ,PR,R):- Q =  ['what', 'can', 'i', 'have', 'for', M,'that', 'contains', I,'?'] ,
getUnlikedIngredients(PQ,FL),
bagof([X],(prop(X,contain,I),isMeal(X,M),isLiked(X,FL),isBelowLimit(X,PQ,PR)),CR),
getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,R):- Q =  ['what', 'can', 'i', 'have', 'for', M,'that', 'contains', I,'?'] ,
getUnlikedIngredients(PQ,FL),
not(bagof([X],(prop(X,contain,I),isMeal(X,M),isLiked(X,FL),isBelowLimit(X,PQ,PR)),CR)),
prop(I,is,_),
CR =  [['Nothing', from, what, 'I', know]],
getDiffAnswer(Q,PQ,PR,CR,R).


response(Q,PQ,PR,R):- Q =  ['what', 'can', 'i', 'have', 'for', M,'that', 'contains', I,'?'] ,
%not(bagof([X],(prop(X,contain,I),isMeal(X,M)),CR)),
not(prop(I,is,_)),
CR =  [['I',do,not,know]],
getDiffAnswer(Q,PQ,PR,CR,R).


%%%% ALL %%%%%%

response(Q,_,_,R):- (Q = ['i', ate, _, for, _, '.'] ; Q = ['i', 'do', 'not', eat, _, '.'] ),
R = ["Ok"].


response(Q,_,_,R):- not(isValid(Q)),
R = ['I', can ,not ,understand, you].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deleteList([],R,R).

deleteList([ELH|ELT],L,R):- delete(L,ELH,Res),
                            deleteList(ELT,Res,R).


listOrderDesc(LP,OLP):- listOrderDesc_helper(LP,OLP,[]).

listOrderDesc_helper([],OLP,OLP).

listOrderDesc_helper(LP,OLP,Acc):-
                                  bestMatchesOcc(LP,BL),
                                  deleteList(BL,LP,Res),
                                  append(Acc,BL,Acc1),
                                  listOrderDesc_helper(Res,OLP,Acc1) .
                        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

foodFromHistory(HL,FL):- foodFromHistory_helper(HL,FL,[]).

foodFromHistory_helper([],FL,FL).

foodFromHistory_helper([HLH|HLT],FL,Acc):- ( HLH = ['i' ,ate, T, for, _,'.'] ; HLH = ['You',can,have,T,for,_] ),
append(Acc,[T],Acc1),
foodFromHistory_helper(HLT,FL,Acc1).

foodFromHistory_helper([HLH|HLT],FL,Acc):- ( HLH \= ['i' ,ate, T, for, _,'.'] , HLH \= ['You',can,have,T,for,_] ),
foodFromHistory_helper(HLT,FL,Acc).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

foodFromHistoryWithMeal(HL,FL):- foodFromHistoryWithMeal_helper(HL,FL,[]).

foodFromHistoryWithMeal_helper([],FL,FL).

foodFromHistoryWithMeal_helper([HLH|HLT],FL,Acc):- ( HLH = ['i' ,ate, T, for, M,'.'] ; HLH = ['You',can,have,T,for,M] ),
%prop(T,contain,_), !,
append([[T,M]],Acc,Acc1),
foodFromHistoryWithMeal_helper(HLT,FL,Acc1).

foodFromHistoryWithMeal_helper([HLH|HLT],FL,Acc):- ( HLH \= ['i' ,ate, T, for, _,'.'] , HLH \= ['You',can,have,T,for,_] ),
foodFromHistoryWithMeal_helper(HLT,FL,Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getUnlikedIngredients(PQ,FL):- getUnlikedIngredients_helper(PQ,FL,[]).

getUnlikedIngredients_helper([],Acc,Acc).

getUnlikedIngredients_helper([PQH|PQT],FL,Acc) :- PQH = ['i', do, not, eat, I,'.'],
append(Acc,[I],Acc1),
getUnlikedIngredients_helper(PQT,FL,Acc1).

getUnlikedIngredients_helper([PQH|PQT],FL,Acc) :- PQH \= ['i', do, not, eat, I,'.'],
getUnlikedIngredients_helper(PQT,FL,Acc).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
