module Halogen.HTML.Renderer.VirtualDOM
  ( renderHTML
  ) where

import Prelude

import Control.Bind ((=<<))
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Data.Exists (runExists)
import Data.ExistsR (runExistsR)
import Data.Foldable (foldl, foldMap)
import Data.Function (runFn2)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Nullable (toNullable)

import Halogen.Effects (HalogenEffects())
import Halogen.HTML.Core (HTML(..), Prop(..), PropF(..), HandlerF(..), runNamespace, runTagName, runPropName, runAttrName, runEventName)
import Halogen.HTML.Events.Handler (runEventHandler)
import qualified Halogen.Internal.VirtualDOM as V
import Halogen.Internal.Address (Address(), runAddress, Send(), runSend)

import Unsafe.Coerce (unsafeCoerce)

-- | Render a `HTML` document to a virtual DOM node
-- |
-- | The first argument is an event handler.
renderHTML :: forall p. HTML p Send -> V.VTree
renderHTML = go
  where
  go :: HTML p Send -> V.VTree
  go (Text s) = V.vtext s
  go (Element ns name props els) =
      let ns' = toNullable $ runNamespace <$> ns
          tag = runTagName name
          key = toNullable $ foldl findKey Nothing props
      in V.vnode ns' tag key (foldMap renderProp props) (map go els)
  go (Slot _) = V.vtext ""

renderProp :: Prop Send -> V.Props
renderProp (Prop e) = runExists (\(PropF key value _) ->
  runFn2 V.prop (runPropName key) value) e
renderProp (Attr ns name value) =
  let attrName = maybe "" (\ns' -> runNamespace ns' <> ":") ns <> runAttrName name
  in runFn2 V.attr attrName value
renderProp (Handler e) = runExistsR (\(HandlerF name k) ->
  runFn2 V.handlerProp (runEventName name) \ev -> runEventHandler ev (k ev) >>= maybe (pure unit) handleSend) e
renderProp (Initializer f) = V.initProp (handleSend <<< f)
renderProp (Finalizer f) = V.finalizerProp (handleSend <<< f)
renderProp _ = mempty

handleAff :: forall eff a. Aff (HalogenEffects eff) a -> Eff (HalogenEffects eff) Unit
handleAff = runAff throwException (const (pure unit))

handleSend :: forall eff. Send -> Eff (HalogenEffects eff) Unit
handleSend = runSend \f -> maybe (pure unit) (runAddress \dr -> handleAff (unsafeCoerce (dr (f unit))))

findKey :: forall i. Maybe String -> Prop i -> Maybe String
findKey _ (Key k) = Just k
findKey r _ = r
