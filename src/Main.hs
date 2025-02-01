{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Servant.HTML.Blaze
import Network.Wai.Handler.Warp (run)
import Text.Blaze.Html
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H

-- Define our API type
type API = 
    "home" :> Get '[HTML] Html
    :<|> "about" :> Get '[HTML] Html
    :<|> "events" :> Get '[HTML] Html
    :<|> "join" :> Get '[HTML] Html

-- Common page template
pageTemplate :: String -> Html -> Html
pageTemplate title content = H.docTypeHtml $ do
    H.head $ do
        H.meta ! charset "UTF-8"
        H.title $ H.toHtml title
        H.link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css"
    H.body ! class_ "bg-gray-100" $ do
        H.nav ! class_ "bg-white shadow mb-8" $ do
            H.div ! class_ "max-w-4xl mx-auto px-8 py-4" $ do
                H.a ! href "/home" ! class_ "text-lg font-semibold text-gray-800 hover:text-gray-600" $ "Home"
                H.a ! href "/about" ! class_ "ml-6 text-lg font-semibold text-gray-800 hover:text-gray-600" $ "About"
                H.a ! href "/events" ! class_ "ml-6 text-lg font-semibold text-gray-800 hover:text-gray-600" $ "Events"
                H.a ! href "/join" ! class_ "ml-6 text-lg font-semibold text-gray-800 hover:text-gray-600" $ "Join Us"
        H.div ! class_ "max-w-4xl mx-auto p-8" $ content

-- Home page
homePage :: Html
homePage = pageTemplate "Home" $ do
    H.h1 ! class_ "text-4xl font-bold mb-8" $ "Cambridge University Defense Tech Society"
    H.p ! class_ "text-lg mb-4" $
        "Welcome to the official website of the Cambridge University Defense Tech Society."
    H.p ! class_ "text-lg mb-4" $
        "Explore our website to learn more about our mission, upcoming events, and how to join."

-- About page
aboutPage :: Html
aboutPage = pageTemplate "About Us" $ do
    H.h1 ! class_ "text-4xl font-bold mb-8" $ "About Us"
    H.p ! class_ "text-lg mb-4" $
        "The Cambridge University Defense Tech Society brings together students from various disciplines to discuss, research, and innovate in the field of defense technology."
    H.p ! class_ "text-lg mb-4" $
        "Our members come from engineering, computer science, physics, and other STEM backgrounds, united by our interest in defense technology and its applications."

-- Events page
eventsPage :: Html
eventsPage = pageTemplate "Events Calendar" $ do
    H.h1 ! class_ "text-4xl font-bold mb-8" $ "Events Calendar"
    H.p ! class_ "text-lg mb-4" $ "Upcoming events:"
    H.ul ! class_ "list-disc pl-8" $ do
        H.li ! class_ "mb-2" $ "February 15: Guest Lecture on AI in Defense"
        H.li ! class_ "mb-2" $ "March 1: Workshop on Cybersecurity"
        H.li ! class_ "mb-2" $ "March 15: Field Trip to Defense Research Facility"

-- Join page
joinPage :: Html
joinPage = pageTemplate "Join Us" $ do
    H.h1 ! class_ "text-4xl font-bold mb-8" $ "Join Our Society"
    H.p ! class_ "text-lg mb-4" $
        "We welcome students from all backgrounds who are interested in defense technology."
    H.p ! class_ "text-lg mb-4" $
        "To join, please fill out our membership form:"
    H.a ! href "#" ! class_ "bg-blue-500 text-white px-6 py-2 rounded-lg hover:bg-blue-600" $ "Join Now"

-- Define our server implementation
server :: Server API
server = 
    return homePage
    :<|> return aboutPage
    :<|> return eventsPage
    :<|> return joinPage

-- Create the API type
app :: Application
app = serve (Proxy :: Proxy API) server

-- Main entry point
main :: IO ()
main = do
    putStrLn "Starting Cambridge Defense Tech Society web server on port 8080..."
    putStrLn "Available routes:"
    putStrLn "  /home    - Main page"
    putStrLn "  /about   - About us"
    putStrLn "  /events  - Events calendar"
    putStrLn "  /join    - Join us page"
    run 8080 app
