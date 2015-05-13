import Development.Shake
import Development.Shake.FilePath

svgDir, destDir :: String
svgDir = "output"
destDir = "../Graphics"

main :: IO ()
main = shakeArgs shakeOptions $ do
    action $ do
        mds <- getDirectoryFiles svgDir ["//*.svg"]
        need [destDir </> x -<.> "pdf" | x <- mds]

    destDir ++ "//*.pdf" %> \out -> do
        let src = svgDir </> (dropDirectory1 . dropDirectory1) out -<.> "svg"
        need [src]
        cmd "convert" [src, out]
