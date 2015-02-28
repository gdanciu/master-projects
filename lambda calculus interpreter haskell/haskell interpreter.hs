module Asdx where
import Data.Char
import Parser

type Variable  = String 
type Context = [(Variable, Expr)] 

data Expr = Var Variable
			|Fun Variable Expr
			|App Expr Expr
			|Def Variable Expr
			|Cls Expr Context
			|Fnl Variable Expr Context
			deriving (Eq, Show)

-- PARSER
			
var = (spotWhile1 isLetter) `transform` Var

toFun (_, (Var arg, (_, body))) = Fun arg body
fun = ((token '\\') >*> var >*> (token '.') >*> (var `alt` fun `alt` def `alt` app)) `transform` toFun

toDef (Var var, (_, expr)) = Def var expr
def = (var >*> (token '=') >*> (var `alt` fun `alt` def `alt` app)) `transform` toDef

toApp (_, (exp1, (_, (exp2, _)))) = App exp1 exp2
app = (token '(' >*> (var `alt` fun `alt` def `alt` app) >*> (token ' ') >*> (var `alt` fun `alt` def `alt` app) >*> token ')') `transform` toApp

parseline = (var `alt` fun `alt` def `alt` app)

parseOneLine= parseline >*> (maxList ( (spotWhile1 ( == ' ')) `alt` (spotWhile1 (== '\r')) `alt` (spotWhile1 (== '\n'))) )

-- parseall is parseEntireLine repeated any number of times
parseall = maxList parseOneLine

cleanList list = map fst list


-- EVALUATOR
-- start with empty global context
evalTop2 :: [Expr] -> [Expr]
evalTop2 elist = evalAll [] elist
{-
-- global context; start with empty local context
evalAll :: Context -> [Expr] -> [Expr]
evalAll gc [] = []
evalAll gc (ehead : etail) =  (fst (eval gc ehead [])) : (evalAll (snd (eval gc ehead [])) etail)
-}
-- global context; start with empty local context
evalAll :: Context -> [Expr] -> [Expr]
evalAll gc [] = []
evalAll gc (ehead : etail) =  (fst (ev gc ehead )) : (evalAll (snd (ev gc ehead )) etail)

ev :: Context -> Expr -> (Expr, Context)
ev gc x = if (x1 == x) 
		  then (x, gc)
		  else (ev gc1 x1)
	where (x1, gc1) =  (eval gc x [])


-- global context -> Closure
eval :: Context -> Expr -> Context -> (Expr, Context) 
eval gc (Def x e) lc = ((fst (eval gc e lc)), (updContext gc x (fst (eval gc e lc))))	--update global context

eval gc (Var x) lc =  if (retrFromGcAndLc gc lc x) == []  -- evalVar
					  then (Var x, gc)
					  else ((head (retrFromGcAndLc gc lc x)), gc)
							
eval gc (Fun x e) lc = ((Fnl x e lc), gc) -- close

eval gc (App (Fnl x e' lc') e) lc = ((Cls e' (updContext lc' x (Cls e lc))), gc)   -- reduce

eval gc (App e1 e2) lc = ((App (fst (eval gc e1 lc)) e2), gc) -- evalApp

eval gc (Cls a@(Cls _ _) lc') lc  = (a, gc) --strip
eval gc (Cls a@(Fnl x e lc2) lc1) lc = (a, gc) 
						 		 
eval gc (Cls e lc') lc = ((Cls (fst (eval gc e lc')) lc'), gc) --evalCls

eval gc e lc = (e, gc)

--tt = eval [("false", (Fun "x" ( Fun "y" (Var "y")))), ("true", (Fun "x" ( Fun "y" (Var "x"))))] (Cls (App (Var "x") (Var "y")) [("x", (Var "true")), ("y", (Var "false"))] )

-- retrieve from context; one expresion if found or 0 if not found
--retrContext :: Context -> Variable -> [Expr]
retrContext [] x = []
retrContext ((x, e1) : tailc) y = if x == y then [e1]
											else retrContext tailc y

retrFromGcAndLc gc lc x = if (retrContext lc x) == [] then (retrContext gc x)
													  else (retrContext lc x)
											
											
-- update context
--updContext :: Context -> Variable -> Expr -> Context
updContext [] x e = [(x, e)]
updContext ((x, e1) : tailc) y e = if x == y then (x, e) : tailc
											else (x, e1) : (updContext tailc y e)

dispExpr :: Expr -> [Char]
dispExpr (Var x) = x
dispExpr (Fun x e) = "\\" ++ x ++ "." ++ (dispExpr e)
dispExpr (App e1 e2) = "(" ++ (dispExpr e1) ++ " " ++ (dispExpr e2) ++ ")"
dispExpr (Def x e) = dispExpr e
dispExpr (Fnl x e c) = "\\" ++ x ++ "." ++ (dispExpr e)
dispExpr (Cls e c) = dispExpr e 
dispExpr x = show x
					
dispExprForContext :: Expr -> [Char]
dispExprForContext (Cls e c) = dispExpr e
dispExprForContext e = dispExpr e
	
dispContext [] = []
dispContext ((x,e) : [] ) = x ++ " <- " ++ (dispExprForContext e)
dispContext ((x,e) : t) = x ++ " <- " ++ (dispExprForContext e) ++ ", " ++ (dispContext t)
											
disp [] = ""
disp ((Cls e c) : t) = "<" ++ (dispExpr e) ++ "; " ++ (dispContext c)++ ">\n" ++ (disp t)
disp (e : t) = "<" ++ (dispExpr e) ++ ";>\n" ++ (disp t) -- "<" ++ (dispExpr e) ++ "; " ++ (dispContext c) ++ ">\n" ++ (disp t)
test = (readFile "input.in") >>= putStr. disp . evalTop2. cleanList . (parse parseall)

--Proiect 5 Type Synthesizer for the Polymorphic Lambda Calculus--
type TVariable = String
data Type = TVar TVariable
		   |TFun Type Type
		   deriving (Eq, Show)
type TContext = [(Variable, Type)]
type Subst = [(TVariable, Type)]

e1 ( x, _, _, _) = x
e2 (_, x, _, _) = x
e3 (_, _, x, _) = x
e4 (_, _, _, x) = x

dispT :: Type -> [Char]
dispT (TVar x) = x
dispT (TFun x e) = "(" ++ (dispT x) ++ "->" ++ (dispT e) ++ ")"

dispTArray :: [Type] -> [Char]
dispTArray [] = []
dispTArray (h:t) = (dispT h) ++ "\n" ++(dispTArray t)

newT nr = "t" ++ (show nr)

occurs :: TVariable -> Type -> Bool
occurs x (TVar y) = (x == y)
occurs x (TFun y e) = (occurs x y) || (occurs x e)

--replaces TVariable with Type1 in Type2
replaceTVar :: TVariable -> Type -> Type -> Type
replaceTVar x t (TVar y) = if x == y then t
									else (TVar y)
replaceTVar x t (TFun y e) = (TFun (replaceTVar x t y) (replaceTVar x t e))
  
replaceAll :: Subst -> TVariable -> Type -> Subst
replaceAll [] x e = []
replaceAll ((sx, se) : stail) x e = (sx, (replaceTVar x e se)) : (replaceAll stail x e)
 
-- must pay attention not to add cycles in subst
updSubst :: Subst -> TVariable -> Type -> Subst
updSubst s x e = if (substEnd s x) == (TVar x) -- x is not in s 
				 then (x, e) : (replaceAll s x e) 
				 else (if fst (unify s (substEnd s x) e) 
					   then snd (unify s (substEnd s x) e)
					   else error "not unifying1")
					   
unify :: Subst -> Type -> Type -> (Bool, Subst)
unify s t1 t2 = if (substEnd2 s t1) == (substEnd2 s t2)
				then (True, s)
				else unify2 s (substEnd2 s t1) (substEnd2 s t2)

-- assumes t1 and t2 do not have the same end, so unification must be done
unify2 :: Subst -> Type -> Type -> (Bool, Subst)
unify2 s (TVar v1) (TVar v2) = (True, updSubst s v1 (TVar v2)) --
unify2 s (TVar v1) f@(TFun _ _) = if (occurs v1 f)
								  then error "not unifying2"
								  else (True, updSubst s v1 f)
unify2 s f@(TFun _ _) (TVar v1) = unify2 s (TVar v1) f
unify2 s f1@(TFun x1 e1) f2@(TFun x2 e2) = if fst (unify s x1 x2)
								  then (if fst (unify s e1 e2)
										then unify (snd (unify s x1 x2)) e1 e2
										else error "not unifying3")
								  else error "not unifying4"								  

-- 
substEnd :: Subst -> TVariable -> Type
substEnd [] t = (TVar t)
substEnd ((xs, types) : tail) x = if x == xs then types
										     else substEnd tail x
											 
substEnd2 :: Subst -> Type -> Type
substEnd2 s (TVar t) = substEnd s t
substEnd2 s (TFun x e) = (TFun (substEnd2 s x) (substEnd2 s e))



s = [("t1", (TFun (TVar "x1") (TVar "t2")))]
e = unify s (TVar "t1") (TFun (TVar "t2") (TVar "x1"))

type2int :: TVariable -> Integer
type2int x = read (drop 1 x) :: Integer

copyT :: Type -> Integer -> (Type, Integer)
copyT x nr = copyT2 x nr (varsInTypeNoDuplicates x)

copyT2 :: Type -> Integer -> [TVariable] -> (Type, Integer)
copyT2 t nr [] = (t, nr)
copyT2 t nr (h:tail) = copyT2 (replaceTVar h (TVar (newT nr)) t) (nr+1) tail 

contains :: [TVariable] -> TVariable -> Bool
contains [] v = False
contains (h:t) v = ((h == v) || (contains t v))

varsInTypeWithDuplicates :: Type -> [TVariable]
varsInTypeWithDuplicates (TVar x) = [x]
varsInTypeWithDuplicates (TFun x f) = (varsInTypeWithDuplicates x) ++ (varsInTypeWithDuplicates f)

clearDuplicates :: [TVariable] -> [TVariable]
clearDuplicates x = clearDuplicates2 x []

--to mantain the order in the vector, we need to keep the first occurence and remove the last ones
clearDuplicates2 :: [TVariable] -> [TVariable] -> [TVariable]
clearDuplicates2 [] ret = ret
clearDuplicates2 (h :t) ret = if (contains ret h) then clearDuplicates2 t ret
						 else clearDuplicates2 t (ret ++ [h]) 

varsInTypeNoDuplicates :: Type -> [TVariable]
varsInTypeNoDuplicates t = clearDuplicates (varsInTypeWithDuplicates t)

retrTypeFromGcAndLc :: TContext -> TContext -> TVariable -> Integer -> ([Type], Integer)
retrTypeFromGcAndLc gc lc x nr = if ((retrContext lc x) == [])
								 then (if (retrContext gc x) == []
									   then ([], nr)
									   else ([fst copy], (snd copy)))
								 else ([(head (retrContext lc x))], nr)
								 where copy = copyT (head (retrContext gc x)) nr

-- unify 
unifyWithNewFunction evalRet nr = unify (e3 evalRet) (TFun (TVar (newT nr)) (TVar (newT (nr+1)))) (e1 evalRet) 

-- from (Global Context, Substitution, Expr, LocalContext) -> ( Type, GlobalContext, Subst, nr)
evalT :: TContext -> Subst -> Expr -> TContext -> Integer -> (Type, TContext, Subst, Integer)
evalT gc s (Def x e) lc nr = ( (e1 eeval), (updContext gc x (e1 eeval)), (e3 eeval), ((e4 eeval)+1))--useless increase
						where eeval = (evalT gc s e lc nr)
						
evalT gc s (Var x) lc nr =  if (fst retr) == []  -- evalVar
						    then ( (TVar ("t" ++ (show nr))), gc, s, nr + 1)
						    else ( (substEnd2 s (head (fst retr))), gc, s, (snd retr))
							where retr = (retrTypeFromGcAndLc gc lc x nr)
	
evalT gc s (Fun x e) lc nr = ( (TFun (substEnd2 (e3 eeval) (TVar (newT nr))) (e1 eeval)), gc, (e3 eeval), (e4 eeval)) 
							 where eeval = (evalT gc s e (updContext lc x (TVar (newT nr))) (nr+1))

	
evalT gc s (App x1 x2) lc nr = ( (substEnd (snd subst2) (newT (functionVarNr+1))), gc, (snd subst2), (e4 evalx2))	-- TApp1
					where 
					  evalx1 = (evalT gc s x1 lc nr);
					  functionVarNr = (e4 evalx1);
					  substToMakeX1Function = (snd (unifyWithNewFunction evalx1 functionVarNr));
					  evalx2 = (evalT gc substToMakeX1Function x2 lc (functionVarNr + 2)); --need to increase the counter by 2 because of newFunction
					  subst2 = (unify (e3 evalx2) (TVar (newT functionVarNr)) (e1 evalx2))

{-g = [("true", (TFun (TVar "t6") (TFun (TVar "t7") (TVar "t6")))), ("false", (TFun (TVar "t9") (TFun (TVar "t10") (TVar "t10"))))]

expr = cleanList (parseline "(x false)")
	
cc = evalT g [] (head expr) [] 12
co = retrTypeFromGcAndLc g [] "false" 15
dispSubst [] = []
dispSubst (h:t) = (show (fst h)) ++ "=" ++(dispT (snd h)) ++ "\n" ++ (dispSubst t)
d x = putStr (dispSubst x)
-}
evalAll2 :: TContext -> [Expr] -> Integer -> [Type]
evalAll2 gc [] nr = []
evalAll2 gc (ehead : etail) nr = (e1 ret) : (evalAll2 (e2 ret) etail (e4 ret))
		where ret = (evalT gc [] ehead [] nr )
		
evalTop3 :: [Expr] -> [Type]
evalTop3 elist = evalAll2 [] elist 0
test2 = (readFile "input.in") >>= putStr. dispTArray . evalTop3. cleanList . (parse parseall)
