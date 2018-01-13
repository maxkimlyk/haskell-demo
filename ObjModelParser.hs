module ObjModelParser (
    Model(..), readModel, centrizeModel, modelToVertexList
    ) where

import Graphics.Rendering.OpenGL hiding (Texture, normalize)
import Data.List.Split
import Data.Maybe

type Vector2d = Vector2 Double
type Vector3i = Vector3 Int
type Vector3d = Vector3 Double

data ParseResult = Vertex Vector3d | Normal Vector3d | Texture Vector2d | Face Vector3i Vector3i Vector3i | NoParseResult

data Model = Model {
    vertices :: [Vector3d],
    normals :: [Vector3d],
    textures :: [Vector2d],
    faces :: [(Vector3i, Vector3i, Vector3i)]
    } deriving Show

readModel :: FilePath -> IO Model
readModel filename = do
    fileContent <- readFile filename
    let lines = splitOn "\n" fileContent
    let model = foldl parseLineAndAddToModel (Model [] [] [] []) lines
    return $ normalizeModel model

    where
        addToModel model Nothing = model
        addToModel model (Just parseResult) =
            case parseResult of
                Vertex vec          -> model {vertices = (vertices model) ++ [vec]}
                Normal vec          -> model {normals = (normals model) ++ [vec]}
                Texture vec         -> model {textures = (textures model) ++ [vec]}

                Face indv@(Vector3 i1 i2 i3) (Vector3 0 0 0) indt ->
                    model {normals = (normals model) ++ [norm, norm, norm], faces = (faces model) ++ [(indv, indn, indt)]} where
                        len = length (normals model)
                        indn = Vector3 (len+1) (len+2) (len+3)
                        norm = normalize $ crossProduct vec1 vec2
                        p1 = (vertices model) !! (i1 - 1)
                        p2 = (vertices model) !! (i2 - 1)
                        p3 = (vertices model) !! (i3 - 1)
                        vec1 = p1 -~- p3
                        vec2 = p2 -~- p1

                Face indv indn indt -> model {faces = (faces model) ++ [(indv, indn, indt)]}
                _                   -> model

        parseLineAndAddToModel model line = addToModel model (parseString line)

parseString :: String -> Maybe ParseResult
parseString string =
    let ws = wordsWhen isSpace isSeparator string
        isSeparator = (== '/')
        isSpace x = x == ' ' || x == '\n' || x == '\t'
    in case ws of
        ["v", x, y, z] -> do
            vec <- parseVector3 [x, y, z]
            return $ Vertex vec
        ["vn", x, y, z] -> do
            vec <- parseVector3 [x, y, z]
            return $ Normal vec
        ["vt", u, v] -> do
            vec <- parseVector2 [u, v]
            return $ Texture vec
        ["f", v1, "/", vt1, "/", vn1, v2, "/", vt2, "/", vn2, v3, "/", vt3, "/", vn3] -> do
            indv <- parseVector3 [v1, v2, v3]
            indn <- parseVector3 [vn1, vn2, vn3]
            indt <- parseVector3 [vt1, vt2, vt3]
            return $ Face indv indn indt
        ["f", v1, "/", "/", vn1, v2, "/", "/", vn2, v3, "/", "/", vn3] -> do
            indv <- parseVector3 [v1, v2, v3]
            indn <- parseVector3 [vn1, vn2, vn3]
            return $ Face indv indn (Vector3 0 0 0)
        ["f", v1, "/", vt1, v2, "/", vt2, v3, "/", vt3] -> do
            indv <- parseVector3 [v1, v2, v3]
            indt <- parseVector3 [vt1, vt2, vt3]
            let indn = Vector3 0 0 0
            return $ Face indv indn indt
        _ -> Nothing

vector2FromCoordList :: [a] -> Maybe (Vector2 a)
vector2FromCoordList [x, y] = Just $ Vector2 x y
vector2FromCoordList _      = Nothing

vector3FromCoordList :: [a] -> Maybe (Vector3 a)
vector3FromCoordList [x,y,z] = Just $ Vector3 x y z
vector3FromCoordList _       = Nothing

parseVector2 :: (Eq a, Read a) => [String] -> Maybe (Vector2 a)
parseVector2 words = do
    numbers <- parseVector words
    vector2FromCoordList numbers

parseVector3 :: (Eq a, Read a) => [String] -> Maybe (Vector3 a)
parseVector3 words = do
    numbers <- parseVector words
    vector3FromCoordList numbers

parseVector :: (Eq a, Read a) => [String] -> Maybe [a]
parseVector words = do
    let readed = map reads words
    if any (== []) readed then
        Nothing
    else do
        let numbers = map (fst . head) readed
        return numbers

wordsWhen :: (Char -> Bool) -> (Char -> Bool) -> String -> [String]
wordsWhen isSpace isSeparator str =
    case dropWhile isSpace str of
        "" -> []
        (first:tail) | isSeparator first -> [first] : wordsWhen isSpace isSeparator tail
        str' -> w : wordsWhen isSpace isSeparator str''
            where (w, str'') = break (\x -> isSpace x || isSeparator x) str'


modelToVertexList :: Model -> [(Vector3d, Vector3d, Vector3d, Vector3d, Vector3d, Vector3d)]
modelToVertexList (Model vertices normals _ faces) =
    map indexToCoord faces
    where
        indexToCoord ((Vector3 i1 i2 i3), (Vector3 n1 n2 n3), _) =
            (vertices !! (i1-1), vertices !! (i2-1), vertices !! (i3-1),
             normals !! (n1-1),  normals !! (n2-1),  normals !! (n3-1))

normalizeModel :: Model -> Model
normalizeModel model =
    let verts = vertices model
        maxCoord (Vector3 x y z) = (abs x) `max` (abs y) `max` (abs z)
        maxAbsCoord = maximum $ map maxCoord verts
        normalizeCoords (Vector3 x y z) = Vector3 (x / maxAbsCoord) (y / maxAbsCoord) (z / maxAbsCoord)
    in model {vertices = (map normalizeCoords verts)}

centrizeModel :: Model -> Model
centrizeModel model =
    let verts = vertices model

        (center, _) = foldl ff (Vector3 0 0 0, 0) verts

        (_, 0) `ff` v = (v, 1)
        (Vector3 mx my mz, n) `ff` (Vector3 x y z) =
            (Vector3 (n / (n+1) * mx + x / (n+1)) (n / (n+1) * my + y / (n+1)) (n / (n+1) * mz + z / (n+1)) , n+1)

    in model {vertices = (map (-~- center) verts)}

crossProduct :: (Num a) => (Vector3 a) -> (Vector3 a) -> (Vector3 a)
crossProduct (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
    Vector3 (y1*z2 - z1*y2) (z1*x2 - x1*z2) (x1*y2 - y1*x2)

vectorLength :: (Num a, Floating a) => (Vector3 a) -> a
vectorLength (Vector3 x y z) = sqrt $ x ^ 2 + y ^ 2 + z ^ 2

normalize :: Floating a => Vector3 a -> Vector3 a
normalize v@(Vector3 x y z) = let
    length = vectorLength v
    in Vector3 (x / length) (y / length) (z / length)

(-~-) :: Num a => Vector3 a -> Vector3 a -> Vector3 a
(-~-) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
    Vector3 (x1 - x2) (y1 - y2) (z1 - z2)

