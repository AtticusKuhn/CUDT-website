{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Blaze
import Servant.Server.StaticFiles (serveDirectoryWebApp)
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

-- Define our API type
type API =
  Get '[HTML] Html
    :<|> "about" :> Get '[HTML] Html
    :<|> "events" :> Get '[HTML] Html
    :<|> "join" :> Get '[HTML] Html
    :<|> "static" :> Raw

-- Common page template
pageTemplate :: String -> Html -> Html
pageTemplate title content = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset "UTF-8"
    H.title $ H.toHtml title
    H.link ! A.rel "stylesheet" ! A.href "/static/styles.css"
    H.link ! A.rel "icon" ! A.type_ "image/png" ! A.href "/static/defense_tech_logo.png"
  H.body ! A.class_ "bg-gray-50" $ do
    H.nav ! A.class_ "bg-blue-500 p-4" $ do
      H.div ! A.class_ "container mx-auto flex justify-between items-center" $ do
        H.a ! A.href "/" ! A.class_ "text-white font-semibold text-xl" $ "CUDTS"
        H.div $ do
          H.a ! A.href "/" ! A.class_ "text-white hover:text-gray-200 ml-4" $ "Home"
          H.a ! A.href "/about" ! A.class_ "text-white hover:text-gray-200 ml-4" $ "About"
          H.a ! A.href "/events" ! A.class_ "text-white hover:text-gray-200 ml-4" $ "Events"
          H.a ! A.href "/join" ! A.class_ "text-white hover:text-gray-200 ml-4" $ "Join Us"
    H.div ! A.class_ "container mx-auto p-8" $ content

-- Home page
homePage :: Html
homePage = pageTemplate "Home" $ do
  H.h1 ! A.class_ "text-3xl font-bold text-gray-800 mb-4" $ "Cambridge University Defence Tech Society"
  H.p ! A.class_ "text-gray-700 mb-4" $
    "Welcome to the official website of the Cambridge University Defence Tech Society."
  H.p ! A.class_ "text-gray-700" $
    "Explore our website to learn more about our mission, upcoming events, and how to join."

-- About page
aboutPage :: Html
aboutPage = pageTemplate "About Us" $ do
  H.h1 ! A.class_ "text-3xl font-bold text-gray-800 mb-4" $ "About Us"
  H.p ! A.class_ "text-gray-700 mb-4" $
    "The Cambridge University Defence Tech Society brings together students from various disciplines to discuss, research, and innovate in the field of defence technology."
  H.p ! A.class_ "text-gray-700" $
    "Our members come from engineering, computer science, physics, and other STEM backgrounds, united by our interest in defence technology and its applications."

-- Events page
eventsPage :: Html
eventsPage = pageTemplate "Events Calendar" $ do
  H.h1 ! A.class_ "text-3xl font-bold text-gray-800 mb-4" $ "Events Calendar"
  H.p ! A.class_ "text-gray-700 mb-4" $ "Upcoming events:"
  H.ul ! A.class_ "list-disc pl-5" $ do
    H.li ! A.class_ "text-gray-700 mb-2" $ "February 15: Guest Lecture on AI in Defence"
    H.li ! A.class_ "text-gray-700 mb-2" $ "March 1: Workshop on Cybersecurity"
    H.li ! A.class_ "text-gray-700" $ "March 15: Field Trip to Defence Research Facility"

-- Join page
joinPage :: Html
joinPage = pageTemplate "Join Us" $ do
  H.h1 ! A.class_ "text-3xl font-bold text-gray-800 mb-4" $ "Join Our Society"
  H.p ! A.class_ "text-gray-700 mb-4" $
    "We welcome students from all backgrounds who are interested in defence technology."
  H.p ! A.class_ "text-gray-700 mb-4" $
    "To join, please fill out our membership form:"
  H.a ! A.href "#" ! A.class_ "bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600" $ "Join Now"

-- Server implementation
server :: Server API
server =
  return homePage
    :<|> return aboutPage
    :<|> return eventsPage
    :<|> return joinPage
    :<|> serveDirectoryWebApp "static"

-- Application
app :: Application
app = serve (Proxy :: Proxy API) server

-- Main function
main :: IO ()
main = do
  putStrLn "Starting server on port 8080..."
  run 8080 app
