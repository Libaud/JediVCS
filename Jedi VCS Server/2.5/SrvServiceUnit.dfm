object FreeVCSService: TFreeVCSService
  OldCreateOrder = False
  OnCreate = ServiceCreate
  AllowPause = False
  DisplayName = 'FreeVCS AppServer 0.91'
  AfterInstall = ServiceAfterInstall
  AfterUninstall = ServiceAfterUninstall
  OnExecute = ServiceExecute
  OnShutdown = ServiceShutdown
  OnStart = ServiceStart
  OnStop = ServiceStop
  Left = 412
  Top = 287
  Height = 209
  Width = 286
  object AppServer1: TAppServer
    Options = [asoDisplayCommands, asoDisplayClientCount]
    Banner = 'Welcome to FreeVCS DBISAM server'
    Addr = '0.0.0.0'
    Port = '2106'
    ClientTimeout = 30
    TimeoutInterval = 5
    RequestBroker = RequestBroker1
    ListenBacklog = 5
    OnDisplay = AppServer1Display
    OnClientConnected = AppServer1ClientConnected
    OnBeforeSendReply = AppServer1BeforeSendReply
    OnAfterSendReply = AppServer1AfterSendReply
    OnBeforeProcessRequest = AppServer1BeforeProcessRequest
    Left = 40
    Top = 8
  end
  object RequestBroker1: TRequestBroker
    UserData = 0
    Options = [rboDisplayObjectCount]
    Left = 40
    Top = 56
  end
  object UpTimer: TTimer
    Enabled = False
    OnTimer = UpTimerTimer
    Left = 112
    Top = 8
  end
end
