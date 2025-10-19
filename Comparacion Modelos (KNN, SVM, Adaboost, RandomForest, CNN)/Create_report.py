import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import base64
import io

class DataReport:

    def __init__(self, filename):
        self.filename = filename
        self.data = pd.read_excel(filename, engine='openpyxl')

    def generate_html(self):
        html_file = open('report.html', 'w')
        html_file.write("<html><head><title>Análisis Exploratorio de Datos</title><style>table, th, td {border: 1px solid black; border-collapse: collapse;} th, td {padding: 10px;} .graph {display: none;}</style></head><body>\n")
        html_file.write("<h1>Análisis Exploratorio de Datos</h1>\n")
        html_file.write("""
        <script>
        function toggle(id) {
            var graphs = document.getElementsByClassName('graph');
            for (var i = 0; i < graphs.length; i++) {
                graphs[i].style.display = 'none';
            }
            document.getElementById(id).style.display = 'block';
        }
        </script>
        \n""")
        
        html_file.write("<table>\n<tr><th>Variable</th><th>Count</th><th>Mean</th><th>Std</th><th>Min</th><th>25%</th><th>50%</th><th>75%</th><th>Max</th><th>Controls</th></tr>\n")

        for index, column in enumerate(self.data.columns):
            if pd.api.types.is_numeric_dtype(self.data[column]):
                stats = self.data[column].describe()
                stats_row = f"<td>{stats['count']}</td><td>{stats['mean']}</td><td>{stats['std']}</td><td>{stats['min']}</td><td>{stats['25%']}</td><td>{stats['50%']}</td><td>{stats['75%']}</td><td>{stats['max']}</td>"

                # Generate Histogram
                fig, ax = plt.subplots(figsize=(10, 6))
                sns.histplot(self.data[column], bins=30, kde=True, color='skyblue', ax=ax)
                fig_html = io.BytesIO()
                plt.savefig(fig_html, format='png')
                plt.close(fig)
                fig_html.seek(0)
                fig_data_png = base64.b64encode(fig_html.getvalue()).decode('utf8')

                # Generate Boxplot if applicable
                fig_data_png_box = ""
                if 'attrition_flag' in self.data.columns:
                    fig, ax = plt.subplots(figsize=(10, 6))
                    sns.boxplot(x='attrition_flag', y=column, data=self.data, palette='Blues', ax=ax)
                    fig_html = io.BytesIO()
                    plt.savefig(fig_html, format='png')
                    plt.close(fig)
                    fig_html.seek(0)
                    fig_data_png_box = base64.b64encode(fig_html.getvalue()).decode('utf8')

                # Add control buttons
                button_code = f"<button onclick=\"toggle('hist_{index}')\">Histogram</button>"
                if fig_data_png_box:
                    button_code += f"<button onclick=\"toggle('box_{index}')\">Boxplot</button>"
                html_file.write(f"<tr><td>{column}</td>{stats_row}<td>{button_code}</td></tr>\n")
                html_file.write(f"<tr><td colspan='10'><div id='hist_{index}' class='graph'><img src='data:image/png;base64,{fig_data_png}'></div><div id='box_{index}' class='graph' style='display:none;'><img src='data:image/png;base64,{fig_data_png_box}'></div></td></tr>\n")
        for index, column in enumerate(self.data.columns):
            if pd.api.types.is_object_dtype(self.data[column]):
                # First histogram
                fig, ax = plt.subplots(figsize=(10, 6))
                sns.countplot(x=self.data[column], palette="Blues", ax=ax)
                ax.set_title(f'Distribution of {column}')
                fig_html = io.BytesIO()
                plt.savefig(fig_html, format='png')
                plt.close(fig)
                fig_html.seek(0)
                fig_data_png = base64.b64encode(fig_html.getvalue()).decode('utf8')

                # Second histogram with attrition_flag
                fig, ax = plt.subplots(figsize=(10, 6))
                sns.countplot(x=self.data[column], hue=self.data['attrition_flag'], palette="Blues", ax=ax)
                ax.set_title(f'Distribution of {column} by Attrition Flag')
                fig_html = io.BytesIO()
                plt.savefig(fig_html, format='png')
                plt.close(fig)
                fig_html.seek(0)
                fig_data_png_hue = base64.b64encode(fig_html.getvalue()).decode('utf8')

                # Print stats and control buttons
                unique_values = self.data[column].nunique()
                top_values = self.data[column].value_counts().idxmax()
                html_file.write(f"<tr><td>{column}</td><td>{self.data[column].count()}</td><td>{unique_values}</td><td>{top_values}</td>")
                html_file.write(f"<td><button onclick=\"toggle('hist_{index}')\">Histogram</button><button onclick=\"toggle('hue_hist_{index}')\">Attrition Histogram</button></td></tr>\n")
                html_file.write(f"<tr><td colspan='5'><div id='hist_{index}' class='graph'><img src='data:image/png;base64,{fig_data_png}'></div><div id='hue_hist_{index}' class='graph' style='display:none;'><img src='data:image/png;base64,{fig_data_png_hue}'></div></td></tr>\n")
           

        html_file.write("</table>\n")
        html_file.write("</body></html>\n")
        html_file.close()

 