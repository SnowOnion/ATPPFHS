module TryMap where

import qualified Data.Map.Strict as Map
import Data.Map.Merge.Lazy -- merge, mapMaybeMissing, ...

m1 = Map.fromList [(0, "a"), (1, "b"), (3,"c"), (4, "d")]
m2 = Map.fromList [(1, "one"), (2, "two"), (4, "three")]

-- merge 的 value 类型还可以不一样，蒽 有时只需统计是否互相有key吧。
-- merge :: Ord k	 
-- => SimpleWhenMissing k a c	
-- What to do with keys in m1 but not m2
-- -> SimpleWhenMissing k b c	
-- What to do with keys in m2 but not m1
-- -> SimpleWhenMatched k a b c	
-- What to do with keys in both m1 and m2
-- -> Map k a	
-- Map m1
-- -> Map k b	
-- Map m2
-- -> Map k c

{- 重要：
A tactic of type 
SimpleWhenMissing k x z 
is an abstract representation of a function of type 
k -> x -> Maybe z 
.
A tactic of type 
SimpleWhenMatched k x y z 
is an abstract representation of a function of type 
k -> x -> y -> Maybe z 
.
abstract representation 绝不是等价的意思= =、哎这个 DSL 就复杂起来了。当然可以包装。也许下面就有。
-} 
 

m = merge 
    (mapMaybeMissing g1)
    (mapMaybeMissing g2)
    (zipWithMaybeMatched f)
    m1 m2

g1 = \ k x -> Just x -- 只有 m1 有，当然保留
g2 k = Just -- 只有 m2 有，当然保留
f k x y = if x>y then Just x else Just y -- 决定去留的唯一信息是（键 k、在 m1 中的值 x、在 m2 中的值 y）。暂且写“保大的”的。


-- 我好像根本不用大张旗鼓 merge。都是一对键值往 Map 里加。
-- 我这下午在干嘛啊…… 主动撞难题，撞傻去休息吗，，，