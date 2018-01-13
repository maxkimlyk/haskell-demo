import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.Time.Clock.POSIX

import ObjModelParser

getTimestamp :: IO GLdouble
getTimestamp = do
    now <- getPOSIXTime
    return $ fromRational $ toRational now

main = do
    _ <- getArgsAndInitialize
    _ <- createWindow "Graphics Demo"

    tstamp <- getTimestamp
    st <- newIORef (0.0, tstamp)

    modelObj <- readModel "wolf.obj"
    let model = modelToVertexList $ centrizeModel modelObj

    displayCallback $= display st model
    idleCallback $= Just (idle st)

    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    depthFunc          $= Just Less
    light (Light 0)    $= Enabled
    lighting           $= Enabled
    lightModelAmbient  $= Color4 0.3 0.3 0.3 1
    diffuse (Light 0)  $= Color4 0.5 0.5 0.5 1
    blend              $= Enabled
    blendFunc          $= (SrcAlpha, OneMinusSrcAlpha)
    colorMaterial      $= Just (FrontAndBack, AmbientAndDiffuse)

    mainLoop

display st model = do
    (angle, _) <- readIORef st
    clear [ColorBuffer, DepthBuffer]
    color $ Color3 (1 :: GLdouble) 1 1
    position (Light 0) $= Vertex4 0 1 (-10) 1

    preservingMatrix $ do
        scale (0.9::GLdouble) 0.9 0.9
        rotate (angle) (Graphics.UI.GLUT.Vector3 0 1 0)
        let renderList = model
        mapM_ renderTriangle renderList

    flush

idle st = do
    (angle, prevTStamp) <- readIORef st
    tstamp <- getTimestamp
    let rotateSpeed = 50
        dt = tstamp - prevTStamp
        angle' = angle + dt * rotateSpeed
    st $=! (angle', tstamp)
    postRedisplay Nothing

renderTriangle ((Vector3 x1 y1 z1), (Vector3 x2 y2 z2), (Vector3 x3 y3 z3),
                (Vector3 nx1 ny1 nz1), (Vector3 nx2 ny2 nz2), (Vector3 nx3 ny3 nz3)) =
    renderPrimitive Triangles $ do
        normal $ Normal3 nx1 ny1 nz1
        vertex $ Vertex3 x1 y1 z1
        normal $ Normal3 nx2 ny2 nz2
        vertex $ Vertex3 x2 y2 z2
        normal $ Normal3 nx3 ny3 nz3
        vertex $ Vertex3 x3 y3 z3
