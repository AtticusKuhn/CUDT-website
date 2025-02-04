{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.ContentTypes (NoContent (..))
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
    :<|> "sponsors" :> Get '[HTML] Html
    :<|> "static" :> Raw
    :<|> Get '[HTML] Html

navLink :: AttributeValue -> Html -> Html
navLink link text = H.a ! A.href link ! A.class_ "rounded-xl text-chili_red-500 hover:text-jet-200 ml-4" $ text

htmlHead :: String -> Html
htmlHead title = H.head $ do
  H.meta ! A.charset "UTF-8"
  H.title $ H.toHtml title
  H.link ! A.rel "stylesheet" ! A.href "/static/styles.css"
  H.link ! A.rel "icon" ! A.type_ "image/png" ! A.href "/static/defense_tech_logo_2.png"

navLinks :: Html
navLinks = do
  navLink "/about" "About Us"
  navLink "/events" "Events"
  navLink "/join" "Join Us"
  navLink "/sponsors" "Our Sponsors"

navBar :: Html
navBar = do
  H.nav $
    H.div ! A.class_ "container mx-auto flex justify-between items-center" $
      do
        H.div ! A.class_ "flex items-center" $ do
          H.a ! A.href "/" $ H.img ! A.src "/static/defense_tech_logo_2_transparent.png" ! A.class_ "h-12 w-auto"
          navLinks

-- Common page template
pageTemplate :: String -> Html -> Html
pageTemplate title content =
  H.docTypeHtml $ do
    htmlHead title
    H.body ! A.class_ "bg-jet-50 h-screen" $
      H.div ! A.class_ "container mx-auto p-8" $ do
        navBar
        content

heading :: Html -> Html
heading text = H.h1 ! A.class_ "text-3xl font-bold text-jet-800 mb-4" $ text

para :: Html -> Html
para text = H.p ! A.class_ "text-jet-700 mb-4" $ text

homePage :: Html
homePage = pageTemplate "Home" $ do
  heading $ "Cambridge University Defence Tech Society"
  H.p ! A.class_ "text-jet-700 mb-4" $ "Welcome to the official website of the Cambridge University Defence Tech Society."
  H.p ! A.class_ "text-jet-700" $ "Explore our website to learn more about our mission, upcoming events, and how to join."

aboutPage :: Html
aboutPage = pageTemplate "About Us" $ do
  heading $ "About Us"
  para $ "The Cambridge University Defence Tech Society brings together students from various disciplines to discuss, research, and innovate in the field of defence technology."
  H.p ! A.class_ "text-jet-700" $ "Our members come from engineering, computer science, physics, and other STEM backgrounds, united by our interest in defence technology and its applications."

eventsPage :: Html
eventsPage = pageTemplate "Events Calendar" $ do
  heading $ "Events Calendar"
  para $ "Upcoming events:"
  H.ul ! A.class_ "list-disc pl-5" $ do
    H.li ! A.class_ "text-jet-700 mb-2" $ "February 15: Guest Lecture on AI in Defence"
    H.li ! A.class_ "text-jet-700 mb-2" $ "March 1: Workshop on Cybersecurity"
    H.li ! A.class_ "text-jet-700" $ "March 15: Field Trip to Defence Research Facility"

joinPage :: Html
joinPage = pageTemplate "Join Us" $ do
  heading $ "Join Our Society"
  para $ "We welcome students from all backgrounds who are interested in defence technology."
  para $ "To join, please fill out our membership form:"
  H.a ! A.href "#" ! A.class_ "bg-chili_red-500 text-white px-4 py-2 rounded hover:bg-chili_red-600" $ "Join Now"

sponsorsPage :: Html
sponsorsPage = pageTemplate "Sponsors" $ do
  heading $ "Our Sponsors"
  para $ "We are grateful for the support of our sponsors, who help make our events and activities possible."
  H.h2 ! A.class_ "text-2xl font-bold text-jet-800 mb-2" $ "Current Sponsors"
  H.ul ! A.class_ "list-disc pl-5" $ do
    H.li ! A.class_ "text-jet-700 mb-2" $ "Sponsor 1"
    H.li ! A.class_ "text-jet-700 mb-2" $ "Sponsor 2"
    H.li ! A.class_ "text-jet-700" $ "Sponsor 3"
  H.h2 ! A.class_ "text-2xl font-bold text-jet-800 mt-8 mb-2" $ "Become a Sponsor"
  para $ "If your company is interested in supporting the Cambridge University Defence Tech Society, please contact us at "
  H.a ! A.href "mailto:sponsorship@cudts.org" ! A.class_ "text-chili_red-500 hover:underline" $ "sponsorship@cudts.org"
  para $ "We offer various sponsorship packages that provide visibility and engagement opportunities with our members."

-- type HtmlPage :: Type
type HtmlPage = Html

-- type ServerHtml :: Type
type ServerHtml = ServerT API Handler

htmlServer :: ServerHtml
htmlServer =
  pure homePage
    :<|> pure aboutPage
    :<|> pure eventsPage
    :<|> pure joinPage
    :<|> pure sponsorsPage
    :<|> serveDirectoryWebApp "static"
    :<|> pure homePage

type ServerAPI = ServerT API Handler

server :: ServerAPI
server = htmlServer

type App = Application

app :: App
app = serve (Proxy @API) server

main :: IO ()
main = do
  putStrLn "Starting server on port 8080..."
  run 8080 app
