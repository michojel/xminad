import System.Taffybar
import System.Taffybar.TaffyPager

main :: IO()
main = do
    let pager = taffyPagerNew defaultPagerConfig
    defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
                                          , barPosition = Bottom
                                          }
