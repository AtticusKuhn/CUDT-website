{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Servant.HTML.Blaze
import Network.Wai.Handler.Warp (run)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H

-- Define our API type
type API = Get '[HTML] Html

-- Create HTML content using Blaze
homePage :: Html
homePage = docTypeHtml $ do
    H.head $ do
        meta ! charset "UTF-8"
        title "Cambridge University Defense Tech Society"
        link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css"
    body ! class_ "bg-gray-100" $ do
        div ! class_ "max-w-4xl mx-auto p-8" $ do
            h1 ! class_ "text-4xl font-bold mb-8" $ "Cambridge University Defense Tech Society"
            p ! class_ "text-lg mb-4" $ 
                "Welcome to the official website of the Cambridge University Defense Tech Society. "
                "We are a student-run organization focused on exploring and advancing defense technologies."
            h2 ! class_ "text-2xl font-bold mt-8 mb-4" $ "About Us"
            p ! class_ "text-lg mb-4" $
                "Our society brings together students from various disciplines to discuss, research, "
                "and develop innovative solutions in defense technology."
            h2 ! class_ "text-2xl font-bold mt-8 mb-4" $ "Join Us"
            p ! class_ "text-lg mb-4" $
                "We welcome students from all backgrounds who are interested in defense technology. "
                "Join our mailing list or attend our next event!"
            a ! href "#" ! class_ "bg-blue-500 text-white px-6 py-2 rounded-lg hover:bg-blue-600" $ "Join Now"

-- Define our server implementation
server :: Server API
server = return homePage

-- Create the API type
app :: Application
app = serve (Proxy :: Proxy API) server

-- Main entry point
main :: IO ()
main = do
    putStrLn "Starting Cambridge Defense Tech Society web server on port 8080..."
    run 8080 app
