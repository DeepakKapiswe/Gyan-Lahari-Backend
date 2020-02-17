{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}


module Adapter.HTTP.ProxyServer where


import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.ReverseProxy
import Network.HTTP.Client (Manager)
import Servant.Server (ServerT, Tagged )
import Servant


forwardServer :: Manager -> ServerT Raw m
forwardServer manager = Tagged $ waiProxyTo forwardRequest defaultOnExc manager


forwardRequest :: Request -> IO WaiProxyResponse
forwardRequest _ = 
    pure (WPRProxyDest (ProxyDest "127.0.0.1" 3000))        

