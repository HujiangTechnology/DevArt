## What is DevArt?
DevArt is a develop framework for `KOTLIN`. It provides many utils and components for build APPs swiftly. 

- - -

### Base Components

| Base                      | Type                  |
| :------------------------ | :-------------------- |
| BaseActivity              | Activity              |
| BaseAdapter               | Generic Adapter       |
| BaseAppWidget             | DesktopWidget         |
| BaseClassLoader           | Generic Object Loader |
| BaseCursorLoader          | Loader                |
| BaseDatabase              | Database              |
| BaseDialog                | Dialog-Typed Activity |
| BaseDialogFragment        | Fragment              |
| BaseFragment              | Fragment              |
| BaseFragmentAdapter       | PagerAdapter          |
| BaseFragmentStateAdapter  | PagerAdapter          |
| BaseLoader                | Generic List Loader   |
| BaseMainActivity          | Rotatable Activity    |
| BaseNotifyReceiver        | Receiver              |
| BasePopupActivity         | Activity              |
| BasePopupFragment         | Fragment              |
| BasePreferenceFragment    | Fragment              |
| BaseProvider              | Provider              |
| BaseService               | Service               |
| BaseSlidingActivity       | Activity              |
| BaseTabFragment           | Fragment              |
| EndlessPagerAdapter       | PagerAdapter          |
| FragmentPAgerAdapter      | PagerAdapter          |
| FragmentStatePagerAdapter | PagerAdapter          |
| ViewPagerAdapter          | PagerAdapter          |

### Base Utils

| Util               | Function                             |
| :----------------- | :----------------------------------- |
| AccountUtils       | emails or accounts bind on device    |
| AlarmUtils         | alarm operations                     |
| ApkUtils           | get APK info                         |
| AssetManagerUtils  | add asset path to assetManager       |
| AutobootUtils      | manage the autoboot state            |
| CompareUtils       | easy compare between types           |
| ComponentUtils     | get and manage components in APKs    |
| ConfigUtils        | read and write SharedPreference      |
| CursorUtils        | cursor operations                    |
| DatetimeUtils      | datetime algorithm                   |
| DeviceUtils        | get device info                      |
| DownloadUtils      | download and manage download status  |
| DrawableUtils      | get system drawable                  |
| FileCommandUtils   | special file permissions or commands |
| FileUtils          | read and write files                 |
| HttpUtils          | Http requests and responses          |
| ImageUtils         | image operations                     |
| InputMethodUtils   | show or hide input method            |
| JsonUtils          | json parser                          |
| MessageUtils       | send message with params             |
| MiscUtils          | misc                                 |
| NetworkUtils       | get network status                   |
| PackageParserUtils | notification management              |
| ReflectionUtils    | reflect operations                   |
| ResourceUtils      | get resource anywhere                |
| UIUtils            | UI operations, type convertion       |
| WifiUtils          | wifi manage and connect              |
| ZipUtils           | compress or uncompress files         |

### Visible Components

| Component     | Description                                  |
| :------------ | :------------------------------------------- |
| ArcMenu       | popup menu like PATH                         |
| Badger        | show badge icon on views                     |
| Calendar      | show calendar with customized events         |
| Coverflow     | coverflow image slider                       |
| Cropper       | crop image with conditions                   |
| Draggrid      | item-draggable gridview                      |
| Draglist      | item-draggable listview                      |
| Flip          | flip view like book paging                   |
| FloatWindow   | show a float windows on top of desktop       |
| Flowtext      | show layered text surround the images        |
| Gesturelock   | show a gesture unlock view                   |
| Gif           | play gif images                              |
| Glassbar      | convert ActionBar to glass style             |
| Markdown      | play markdown articles                       |
| Misc          | some views not categoried                    |
| Progress      | show customized progress bar                 |
| PullToRefresh | pull down or up for refresh content          |
| Scroll        | paging scrollview with popback effect        |
| Sliding       | sliding left or right in an Activity         |
| Swipe         | let items in listview can swipe another view |
| TouchImage    | image view with gesture operations           |

### Invisible Components

| Component | Description                                         |
| :-------- | :-------------------------------------------------- |
| Cache     | weak cache for data, files and images               |
| Daemon    | daemon service for un-killable                      |
| Dns       | DNS lookup and operations                           |
| Mutax     | the mutax service and notification                  |
| Secutiry  | signature and common algorithms                     |
| Server    | a simple server container for serve website or APIs |
| Command   | run terminal command                                |

### Usage

**BaseActivity**

 - fun getIcon(): Int `icon in ActionBar`
 - fun replaceFragment(): Fragment `fragment in activity`
 - fun customTheme(): Int `theme resource id`
 - fun getActionBarCanBack(): Boolean `show back button on ActionBar`

**BaseFragment**

 - fun getBarTitle(): Int `title resource id`
 - fun getBarTitleWithPath(): Int `title resource id for horzental layout`
 - fuhn getCustomTitle(): String? `title string, take effect when getBarTitle() return 0`
 - fun initComponents() `mapping components from layout xml`
 - fun initEvents() `bind events for components`
 - fun initLogic() `other logic initialization`
 - fun getFragmentLyoutResId(): Int `layout resource id`
 - fun getMainActivityName(): String? `main activity class name for switch rotation`
 - fun initMenu() `build menu on ActionBar`
 - fun onGetNewArguments(bn: Bundle?) `let other fragments or activities pass parameters to self`
 - fun getFragmentState(): Bundle? `let other fragments or activities know self status`
 
**BaseAdapter**

 - fun getView(position: Int, convertView? View?, parent: ViewGroup?): View? `build adapter item view`
 - fun getValueText(item: T): String? `do filter`

* * *

For more information, please visit the project page on [Github](https://github.com/HujiangTechnology/DevArt)