//
  // Copyright (C) 2013-2018 University of Amsterdam
//
  // This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
  // This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form {
  id: form
  //AddColumnField { name: "simulates"; text: "Simulate new variable: "; fieldWidth: 120; id: simulates; columnType: 1}
  //CheckBox{name: "simulateNow"; id: simulateNow}
  //Button{ text: "simulateNow"; onClicked: { if (simulateNow.checked) simulateNow.checked = false; if(!simulateNow//.checked) simulateNow.checked = true}}
  
  Section{
      expanded: true
    title: "Show Distribution"
    columns: 2
    Group
    {
      title: "Parameters"

      Group{
        DropDown
        {
            name: "parametrization"
            id:   parametrizationChoice
            indexDefaultValue: 0
            label: qsTr("Parameterization")
            values: [
                { label: "μ, σ²", value: "sigma2"},
                { label: "μ, σ",  value: "sigma" },
                { label: "μ, τ²", value: "tau2"  },
                { label: "μ, τ",  value: "tau"   }
              ]
        }

        DoubleField{ name:  "mu"; label: qsTr("μ = "); id: mu; negativeValues: true }
        DoubleField{ name: "varValue"; label: parametrizationChoice.currentText.replace("μ, ", "") + qsTr(" ="); defaultValue: 1; min: 0; id: vars }
      }

    }
    
    Group{
      title: qsTr("Display")
      CheckBox{ label: qsTr("Explanatory text"); name: "explanatoryText"}
      CheckBox{ label: qsTr("Parameters, support, and moments"); name: "formulas" }
      CheckBox
      {
        label: qsTr("Probability density function")
        id: plotPDF
        name: "plotPDF"
        checked: true
        CheckBox{ label: qsTr("Formula"); name: "formulaPDF" }
      }
      
      CheckBox{
        label: qsTr("Cumulative distribution function")
        id: plotCDF
        name: "plotCDF"
        CheckBox{ label: qsTr("Formula"); name: "formulaCDF" }
      }
      CheckBox{
          label: qsTr("Quantile function")
          name: "plotQF"
          CheckBox{ label: qsTr("Formula"); name: "formulaQF" }
      }
    }
    
    Group
    {
        title: qsTr("Options")
        enabled: plotPDF.checked || plotCDF.checked
        DoubleField{ name:  "range"; label: qsTr("Range"); defaultValue: 3; id: range}
        Group
        {
            title: qsTr("Highlight")
            Group
            {
                columns: 2
                CheckBox{ name: "highlightDensity"; label: qsTr("Density"); id: highlightDensity }
                CheckBox{ name: "highlightProbability"; label: qsTr("Probability"); id: highlightProbability }
            }
            RadioButtonGroup
            {
                name: "highlightType"
                title: qsTr("At intervals")
                enabled: highlightDensity.checked || highlightProbability.checked
                RadioButton
                {
                    value: "minmax"; label: qsTr("from"); childrenOnSameRow: true; checked: true
                    DoubleField{ name: "min"; label: ""; afterLabel: qsTr("to"); negativeValues: true; defaultValue: -1}
                    DoubleField{ name: "max"; label: ""; negativeValues: true; defaultValue: 1}
                }

                RadioButton
                {
                    value: "lower"; label: qsTr("from -∞"); childrenOnSameRow: true
                    DoubleField{ name: "lower_max"; label: qsTr("to"); negativeValues: true; defaultValue: 0 }
                }

                RadioButton
                {
                    value: "upper"; label: qsTr("from"); childrenOnSameRow: true
                    DoubleField{ name: "upper_min"; label: ""; afterLabel: qsTr("to ∞"); defaultValue: 0}
                }
            }
        }


    }

  }
  
  Section
  {
      title: qsTr("Generate and Display Data")
      //GroupBox{
      //  ComputedColumnField{ name: "sampleFilterName"; text: "Simulate new variable:"; fieldWidth: 120
      //  value: "Normal(3, 5)"}
        //ComputedColumnsConstructor{visible: false; rCode: "rnorm(n = 10, 1, 1)"}
      //}
      //CheckBox
      //{
      //    name: "drawSamples"; label: qsTr("Generate"); childrenOnSameRow: true; id: drawSamples; checked: true
      //    IntegerField{
      //        name: "sampleSize"
      //        afterLabel: qsTr("samples from Normal(μ = ") + mu.value + parametrizationChoice.currentText.replace("μ", "") + qsTr(" = ") + vars.value + qsTr(")")
      //        defaultValue: 100}
      //}

      Button{ text: qsTr("sample"); enabled: true; id: sample }

      VariablesForm
      {
          height: 100
          visible: true//drawSamples.checked === false
          AvailableVariablesList { name: "allVariables" }
          AssignedVariablesList  { name: "variable"; label: qsTr("Get variable from data set"); allowedColumns: ["scale"]; singleVariable: true }
      }


      CheckBox{ name: "summary"; label: qsTr("Summary Statistics")}

      Group
      {
          title: qsTr("Plots")
          CheckBox{ name: "histogram"; label: qsTr("Histogram with"); childrenOnSameRow: true
            IntegerField{ name: "histogramBins"; afterLabel: qsTr("bins"); defaultValue: 30 }
          }
          CheckBox{ name: "ecdf";      label: qsTr("Empirical CDF") }
      }
  }
  
  Section
  {
      title: qsTr("Estimate Parameters")

      Group
      {
          title: ""
          CheckBox{ name: "methodUnbiased"; label: qsTr("Unbiased estimator")}
          CheckBox{ name: "methodMoments"; label: qsTr("Method of moments") }
          CheckBox
          {
              name: "methodML";      label: qsTr("Maximum Likelihood"); debug: true
              Group{
                  CheckBox{ name: "methodMLAnalytic"; label: qsTr("Analytic")    }
                  CheckBox{ name: "methodMLNewton";   label: qsTr("Newton")      }
                  CheckBox{ name: "methodMLGrid";     label: qsTr("Grid search") }
              }
          }
          CheckBox
          {
              name: "methodBayes";   label: qsTr("Bayesian"); debug: true
              Group{
                  CheckBox{ name: "methodBayesAnalytic"; label: qsTr("Analytic") }
                  CheckBox{ name: "methodBayesMAP";      label: qsTr("Maximum a posteriori") }
                  CheckBox{ name: "methodBayesGibbs";    label: qsTr("Gibbs sampling") }
              }
              Group{title: qsTr("Priors")}
          }
      }

      Group
      {
          title: qsTr("Output")
          debug: false
          CheckBox{ name: "outputEstimates"; label: qsTr("Estimates"); checked: true }

          CheckBox
          { 
              name: "ciInterval"; label: qsTr("Confidence interval"); childrenOnSameRow: true
              PercentField{ name: "ciIntervalInterval"; label: ""; defaultValue: 95}
          }

          Group{
              debug: true
              title: qsTr("Plots")
              CheckBox{ name: "plotEstimates";   label: qsTr("Estimates")   }
              CheckBox{ name: "plotDiagnostics"; label: qsTr("Diagnostics") }
              CheckBox{ name: "plotAdvanced";    label: qsTr("Advanced")    }
          }
      }
  }
  Section
  {
      title: qsTr("Assess Fit")

      Group
      {
          title: qsTr("Plots")
          columns: 2
          CheckBox{ name: "estPDF"; label: qsTr("Estimated p.d.f") }
          CheckBox{ name: "qqplot"; label: qsTr("Q-Q plot")}
          CheckBox{ name: "estCDF"; label: qsTr("Estimated c.d.f") }
          CheckBox{ name: "ppplot"; label: qsTr("P-P plot")}
      }

      Group
      {
          title: qsTr("Statistics")
          CheckBox{ name: "kolmogorovSmirnov"; label: qsTr("Kolmogorov-Smirnov")}
          CheckBox{ name: "cramerVonMisses";   label: qsTr("Cramér–von Mises")  }
          CheckBox{ name: "andersonDarling";   label: qsTr("Anderson-Darling")  }
          CheckBox{ name: "shapiroWilk";        label: qsTr("Shapiro-Wilk")     }
      }
  }
}
