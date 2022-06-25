import AlbumTypes
import Data.Aeson (encode)
import Data.ByteString.Lazy
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [jsonOutput]

jsonOutput :: TestTree
jsonOutput =
  goldenVsString "basic album json" "test/album.json" $ pure makeAlbum

makeAlbum :: ByteString
makeAlbum =
  encode $
    List $
      AlbumList
        { listTitle = "main",
          childFirst =
            Leaf $
              Album
                { title = "album title",
                  thumbnail =
                    Image
                      { altText = "inner thumb text",
                        srcSetFirst =
                          ImgSrc
                            { url = "bar.jpg",
                              x = 3,
                              y = 4
                            },
                        srcSetRest = []
                      },
                  imageFirst =
                    Image
                      { altText = "first image",
                        srcSetFirst =
                          ImgSrc
                            { url = "baz.jpg",
                              x = 5,
                              y = 6
                            },
                        srcSetRest = []
                      },
                  imageRest = []
                },
          childRest = [],
          listThumbnail =
            Image
              { altText = "thumbText",
                srcSetFirst =
                  ImgSrc
                    { url = "foo.jpg",
                      x = 1,
                      y = 2
                    },
                srcSetRest = []
              }
        }