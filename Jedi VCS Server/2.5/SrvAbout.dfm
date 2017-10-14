inherited JVCSSrvAbout: TJVCSSrvAbout
  Caption = 'About JEDI Version Control System Application Server'
  PixelsPerInch = 96
  TextHeight = 14
  inherited PageControl1: TPageControl
    inherited tsAbout: TTabSheet
      inherited Panel2: TPanel
        inherited Label6: TLabel
          Width = 323
          Caption = 'Source codes based on Thomas Hensle'#39's FreeVCS Server.'
        end
      end
    end
    inherited tsCredits: TTabSheet
      inherited Memo3: TJvRichEdit
        Lines.Strings = (
          '')
      end
    end
  end
end
