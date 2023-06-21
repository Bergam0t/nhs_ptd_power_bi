/*
 *  Power BI Visualizations
 *
 *  Copyright (c) Microsoft Corporation
 *  All rights reserved.
 *  MIT License
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the ""Software""), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */

"use strict";

import { dataViewObjectsParser } from "powerbi-visuals-utils-dataviewutils";
import DataViewObjectsParser = dataViewObjectsParser.DataViewObjectsParser;


export class VisualSettings extends DataViewObjectsParser {
      // public rcv_script: rcv_scriptSettings = new rcv_scriptSettings();
      // public ggsettings: 
      //   ChartTitle = new ChartTitle();
      // }
      
      //public ggsettings: ChartTitle = new ChartTitle();

      public spcsettings: SPCSettings = new SPCSettings();
      
      public titlesettings: ChartSettings = new ChartSettings();

      public outputtypesettings: OutputTypeSettings = new OutputTypeSettings();

      public xaxissettings: XAxisSettings = new XAxisSettings();

      public yaxissettings: YAxisSettings = new YAxisSettings();

      public pointsettings: PointSettings = new PointSettings();

      public legendsettings: LegendSettings = new LegendSettings();

      public iconsettings: IconSettings = new IconSettings();

      public cardsettings: CardSettings = new CardSettings();

    }


    // export class rcv_scriptSettings {
    //  // undefined
    //   public provider     // undefined
    //   public source     }

    export class OutputTypeSettings {
      public OutputType: string = "graph"
      }

    export class SPCSettings {
      public ImprovementDirection: string = "increase"
      public Target: number = null;
      }

    export class ChartSettings {
      public TitleOn: boolean = true;
      public ChartTitle: string = "";
      public TitleSize: number = 10;
      public TitleJustification: string = "central";
    }

    export class XAxisSettings {
      public XAxisTitle: string = "";
    }

    export class YAxisSettings {
      public YAxisTitle: string = "";
    }

    export class PointSettings {
      public PointSize: number = 8;
    }

    export class LegendSettings {

      public LegendPosition: string = "below";

    }

    export class IconSettings {

      public IconSize: number = 0.1;

    }

    export class CardSettings {
      public CardSuffix: string = "";
      public CardPrefix: string = "";
      public ValueSize: number = 48;
      public IconPosition: string = "central";
      public CardTitleJustification: string = "central";
      }