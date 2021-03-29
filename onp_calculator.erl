-module(onp).
-export([onp/1, convert/1]).


%% Try to convert firstly to float, if there is an error - convert to int. Otherwise we have an operator.

convert(El) ->
    case string:to_float(El) of
      {error, no_float} -> list_to_integer(El);
      {Num, _} -> Num
    end.

onp(Line) -> push_to_stack(string:tokens(Line, " "), []).

push_to_stack([], [H|_]) -> H; 		%% When there will be no elements in input - the answer is ready
push_to_stack(["+"| T], [A, B | Stack]) -> push_to_stack(T, [A+B | Stack]); 
push_to_stack(["-"| T], [A, B | Stack]) -> push_to_stack(T, [A-B | Stack]); 
push_to_stack(["*"| T], [A, B | Stack]) -> push_to_stack(T, [A*B | Stack]); 
push_to_stack(["/"| T], [A, B | Stack]) -> push_to_stack(T, [A/B | Stack]); 
push_to_stack(["^"| T], [A, B | Stack]) -> push_to_stack(T, [math:pow(A,B) | Stack]); 
push_to_stack(["sqrt"| T], [A | Stack]) -> push_to_stack(T, [math:sqrt(A) | Stack]);  
push_to_stack(["sin"| T], [A | Stack]) -> push_to_stack(T, [math:sin(A) | Stack]); 
push_to_stack(["cos"| T], [A | Stack]) -> push_to_stack(T, [math:cos(A) | Stack]);
push_to_stack(["tg"| T], [A | Stack]) -> push_to_stack(T, [math:tan(A) | Stack]);
push_to_stack(["atg"| T], [A | Stack]) -> push_to_stack(T, [math:atan(A) | Stack]);
push_to_stack(["asin"| T], [A | Stack]) -> push_to_stack(T, [math:asin(A) | Stack]);
push_to_stack(["acos"| T], [A | Stack]) -> push_to_stack(T, [math:acos(A) | Stack]);

% Take the first element and try to convert it
push_to_stack([R | T], Stack) -> push_to_stack(T, [convert(R) | Stack]). 