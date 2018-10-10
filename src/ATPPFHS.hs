module ATPPFHS where

-- http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Strict.html
-- import Data.Map.Strict (Map) -- 一个实践。和下面 qualified 合用。我目前不喜欢……
import qualified Data.Map.Strict as Map

-- Fvar Int rather than Atom Int
-- S for Schema。我想，关于“模式”，还会遇到更多问题……
-- 可替换性深入了解一下……（原子命题可换成任意公式）（e.g. 公开宣告逻辑 PAL 里不成立）
data FormulaS = Fvar Id | Nega FormulaS | FormulaS :~> FormulaS
    deriving(Eq) -- 真·字符串相等，俞会喜欢吧（
type Id = Int -- ZFM 建议。好。这里还能在某种程度上隐藏 Fvar 和 原子命题 的羁绊，因为可以统称 Id
-- TIL: 1. GHC 里中缀的数据构造器要用冒号开头；2. (:~>) 写前缀式，deriving(Show) show 出前缀式；写中缀 show 中缀
infixr 5 :~> -- 5 是拍脑袋想的

-- atom = Fvar
-- “函数中缀化”能规定优先级和结合性吗？-- 我现在不太关心了

-- 人类易读形态。show 者，在多种等价形式之中选择一种~
instance Show FormulaS where
    -- 效率？用 showS？
    show (Fvar i) = 'f':map subscript (show i) where
        subscript dig = toEnum (fromEnum dig - fromEnum '0' + fromEnum '₀')
    show (Nega f) = '¬':show f
    show (f1 :~> f2) = '(':show f1 ++ " → " ++ show f2 ++ ")"
    
{- Semantics -}
-- 跑题弄语义，因为简单……
-- 弄个 class Evaluable，实例有 FormulaS 和 Formula？再说吧。
-- | eval valuation formula(schema)
eval :: (Id -> Bool) -> FormulaS -> Bool
eval v (Fvar i) = v i
eval v (Nega f) = not $ eval v f
eval v (f1 :~> f2) = not (eval v f1) || eval v f2

-- | 功能示例：makeV [True,False,False] 得到函数 {1|→True,2|→False,3|→False,其他|→error}
-- “冗余”括号有点儿暗示作用：此函数在 practice 中往往返回一个函数而不是“最后结果”
makeV :: [Bool] -> (Id -> Bool) 
-- 虽然我用了 strict 的 Map，但这么写，不一定啥时候给构造……TODO 学习关于 strict 的 practice
makeV xs = let vMap = Map.fromAscList (zip [1..] xs) in
    (\i -> vMap Map.! i)

{- /Semantics -}

ax1 = Fvar 1 :~> Fvar 2 :~> Fvar 1
ax2 = (Fvar 1 :~> Fvar 2 :~> Fvar 3) :~> (Fvar 1 :~> Fvar 2) :~> Fvar 1 :~> Fvar 3
ax3 = (Nega (Fvar 2) :~> Nega (Fvar 1)) :~> Fvar 1 :~> Fvar 2


mp :: FormulaS -> FormulaS -> FormulaS
mp a a2b = undefined
-- 换名问题该出现了……

-- | substitute i f2 f1
-- 把 f1 里的变元 i 集体替换成 f2。不 normalize。
-- 这个参数顺序是为了方便和 normalize composite 起来
-- 思考：如果意在和多个函数对接（另包括和测试的 hard coding 数据），那么就得好好权衡参数的形式了。
substitute :: Id -> FormulaS -> FormulaS -> FormulaS
substitute i f2 f1@(Fvar i1)
    | i == i1 = f2
    | otherwise = f1
substitute i f2 (Nega f11) = Nega $ substitute i f2 f11
substitute i f2 (f11 :~> f12) = substitute i f2 f11 :~> substitute i f2 f12

test_substitute_1 = substitute 1 (Fvar 2) ax1
test_substitute_2 = substitute 2 (Fvar 4) ax2
test_substitute_3 = substitute 3 (Fvar 1) ax3

-- | 因为不相信编译器能把多遍 substitute 多个变元处理得像一遍替换多个那么快，所以写个一遍的版本。
-- 当然，有待实验 TODO 以及有没有谁有 Efficient Haskell 写作计划？跟韩冬 etc 取经吧。
-- 以及现在参数专为 normalize 使用简单而设计。否则应该 Map.Map Id FormulaS。
substituteMany :: Map.Map Id Id -> FormulaS -> FormulaS
substituteMany m f1@(Fvar i1) = 
    case Map.lookup i1 m of
        Just i2 -> Fvar i2
        Nothing -> f1
substituteMany m (Nega f11) = Nega $ substituteMany m f11
substituteMany m (f11 :~> f12) = substituteMany m f11 :~> substituteMany m f12
-- 写着写着才觉得应该把 FormulaS 实现成 Functor。

-- | 对公式内的变元做换名，使得各个变元序号递增地第一次出现。
-- 如输入 (Nega (Fvar 2) :~> Nega (Fvar 1)) :~> Fvar 1 :~> Fvar 2
-- ，输出 (Nega (Fvar 1) :~> Nega (Fvar 2)) :~> Fvar 2 :~> Fvar 1
normalize :: FormulaS -> FormulaS
normalize f = substituteMany (genSbstPlan f Map.empty) f where
    -- 先序遍历撸完所有变元，遇到的顺序就是替换目标 -- 更想抽象了……不妨手撸一遍体会抽象要点。
    -- | genSbstPlan f nowMap ，参数 nowMap 为 accumulator……
    genSbstPlan :: FormulaS -> Map.Map Id Id -> Map.Map Id Id
    genSbstPlan (f1 :~> f2) nowMap = 
        let leftMap = genSbstPlan f1 nowMap in
            genSbstPlan f2 leftMap
    genSbstPlan (Fvar i) nowMap
        | Map.null nowMap = Map.singleton i 1 -- magic number
        | otherwise = Map.insertWith (flip const) i (Map.size nowMap + 1) nowMap -- 若已存在则不插入，若不存在则分配一个新编号
    genSbstPlan (Nega f1) nowMap = genSbstPlan f1 nowMap


