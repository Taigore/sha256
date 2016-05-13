import SHA256

main :: IO ()
main = do (putStrLn.show) (sha256 [0x61,0x62,0x63])
