import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# TODO: check for age and log(age)

# Search strategy for latest data

# Legal situation:
#     All except active

# Profit/loss
#     Known 5 years until event

# Total assets
#     Known 5 years until event

# Number of employees
#     Known 5 years until event

# Gupta (2018):
# (1) Leverage
#     Total liabilities / tangible total assets
#     Total liablities / net worth (= net current assets)
# (2) Liquidity
#     Cash and short-term investment / total assets
#         (investements excluded because too restrictive)
#     Cash and marketable securities / current liabilities
#         (securities excluded because too restrictive)
#     Current assets / current liabilities
#     Quick ratio
#         (Quick ratio not available)
# (3) Profitability
#     Earnings before interest
#     Tax
#     EBITDA
#     ROE
#         (replaced by return on total assets)
#     Operating profit / capital employed
#         (not included)
# (4) Financing
#     Financial expenses / total assets
#     Financial expenses / sales
#         (sales not included)
#     Earnings before interest taxes
# (5) Activity
#     Working capital / sales
#         (sales not included)
#     Working capital / total assets
# (6) Growth
#     Different size categories
# (7) Other
#     Income tax / total assets


class BankruptcyDataset:
    def __init__(self, path):
        self.df = self.__read_df(path)


    def __read_df(self, path):
        df = pd.read_csv(path, delimiter="\t", encoding="UTF-16 LE")
        df = df[df["Mark"].notna()]
        df["Mark"] = df["Mark"].fillna(method="ffill")
        df = df.set_index("Mark")

        # Change dtypes where necessary
        net_assets_cols = [i for i in df.columns
                           if "net current assets" in i.lower()]
        for i in net_assets_cols: df[i] = df[i].str.replace(",", ".")
        df[net_assets_cols] = df[net_assets_cols].astype(float)
        return df
    
    
    def plot_event_times(self):
        plt.figure(figsize=(15, 5))
        event_dict = {}
        for i in self.transf_df["status"].unique():
            event_dict[i] = self.transf_df[self.transf_df["status"] == i]\
                                          ["years_to_event"]

        for idx, (i, j) in enumerate(event_dict.items()):
            plt.subplot(1, 3, idx+1)
            plt.hist(j)
            plt.title(i)
            
        plt.savefig("visualizations/hist_event_times.png", dpi=300,
                    bbox_inches="tight")


    def create_transf_df(self):
        """Transforms the raw data from Bel-First to a dataset that is suitable
        for further analysis."""
        self.transf_df = pd.DataFrame(index=self.df.index.unique())
        self.transf_df.index = self.transf_df.index.rename("index")
        self.__add_status()
        self.__add_status_indicator()

        # Only keep companies with the following status
        # self.transf_df = self.transf_df[self.transf_df["status"].isin(
        #     ["Liquidation", "Bankruptcy", "Merger by absorption"]
        # )]
    
        self.__add_leverage_variables()
        self.__add_liquidity_variables()
        self.__add_profitability_variables()
        self.__add_financing_variables()
        self.__add_activity_variables()
        self.__add_growth_variables()
        self.__add_other_variables()
        self.__add_legal_form()

        # Drop columns with Inf, because of division by 0
        print(self.transf_df.shape)
        self.transf_df = self.transf_df.replace([np.Inf, -np.Inf], np.nan)
        self.transf_df = self.transf_df.dropna(how="all")
        print(self.transf_df.shape)


    def __add_status(self):
        """Adds a status, for example bankruptcy, to the transformed dataframe.
        It also calculates a lower bound on the years to that status."""
        # Add status to new df
        self.transf_df["status"] = self.df["Legal situation"]
        replace_map = {"Conclusion of liquidation": "Liquidation",
                       "Reopening liquidation": "Liquidation",
                       "Conclusion of bankruptcy": "Bankruptcy"}
        self.transf_df["status"] = self.transf_df["status"].replace(replace_map)
    
        # Calculate years to event
        self.transf_df["end_date"] = self.df["Legal situation date"]
        self.transf_df["start_date"] = self.df["Date of incorporation"]

        # Replace null values of end date with last known date
        self.transf_df["end_date"] = self.transf_df["end_date"]\
            .fillna(self.df["Last available year"])
    
        self.transf_df["end_date"] = pd.to_datetime(
            self.transf_df["end_date"], dayfirst=True,
        )
        self.transf_df["start_date"] = pd.to_datetime(
            self.transf_df["start_date"], dayfirst=True,
        )
    
        self.transf_df["years_to_event"] = [
            len(pd.date_range(start=i, end=j, freq="Y"))
            for i, j in zip(self.transf_df["start_date"],
                            self.transf_df["end_date"])
        ]
        self.transf_df["years_to_event"] = self.transf_df["years_to_event"]\
            .astype(int)
    
    
    def __add_status_indicator(self):
        """Adds a new column to the transformed dataframe which contains a
        unique int value for each status."""
        indicator_map = self.transf_df["status"].unique().tolist()
        indicator_map = {
            i:j for i, j in
            zip(indicator_map, list(range(len(indicator_map))))
        }
    
        self.transf_df["event_indicator"] = [
            indicator_map[i] for i in self.transf_df["status"]
        ]
    
    
    def __add_mean_pl(self):
        """Adds the mean profit loss in thousands of all profit loss columns
        contained in the original dataframe."""
        cols = [i for i in self.df.columns if "P/L" in i]
        self.df[cols] = self.df[cols].replace("n.a.", np.nan)
        self.transf_df["mean_pl"] = self.df[cols].fillna(0).mean(axis=1)
        self.transf_df["mean_pl"] = self.transf_df["mean_pl"] / 1000
    
    
    def __add_mean_cash_flow(self):
        """Adds the mean cash flow of all cash flow columns
        contained in the original dataframe."""
        cols = [i for i in self.df.columns if "Cash flow" in i]
        self.df[cols] = self.df[cols].replace("n.a.", np.nan)
        self.transf_df["mean_cash_flow"] = self.df[cols].fillna(0).mean(axis=1)
    
    
    def __add_mean_total_assets(self):
        """Adds the mean total assets of all total asset columns
        contained in the original dataframe."""
        cols = [i for i in self.df.columns if "Total assets" in i]
        self.df[cols] = self.df[cols].replace("n.a.", np.nan)
        self.transf_df["mean_total_assets"] = self.df[cols].fillna(0).mean(axis=1)
        self.transf_df["mean_total_assets"] = self.transf_df["mean_total_assets"]\
            / 1000
    
    
    def __add_mean_profit_margin(self):
        """Adds the mean profit margin of all profit margin columns
        contained in the original dataframe."""
        cols = [i for i in self.df.columns if "Profit margin" in i]
        self.df[cols] = self.df[cols].replace("n.a.", np.nan)
        self.transf_df["mean_profit_margin"] = self.df[cols].fillna(0).mean(axis=1)
    
    
    def __add_mean_n_employees(self):
        """Adds the mean number of employees of all relevant columns
        contained in the original dataframe."""
        cols = [i for i in self.df.columns if "employees" in i]
        self.df[cols] = self.df[cols].replace("n.a.", np.nan)
        self.transf_df["mean_n_employees"] = self.df[cols].fillna(0).mean(axis=1)
    
    
    def __add_n_legal_events(self):
        """Adds the number of legal events of the entire lifetime of the
        company."""
        self.transf_df["n_legal_events"] = self.df["Legal events - Date"]\
            .str.count("\n")
        self.transf_df["n_legal_events"] = self.transf_df["n_legal_events"].fillna(0)
        self.transf_df["n_legal_events"] = self.transf_df["n_legal_events"] + 1
    
    
    def __add_peer_group_size(self):
        self.transf_df["peer_group_size"] = self.df["Peer Group Size"]
        return self.transf_df
    
    
    def __add_legal_form(self):
        """Adds a column legal form to the transformed dataframe which
        indicates whether the company is public, private, cooperative, nonprofit
        or of another type."""
        private_list = [
            "Private company with limited liability", "private_company",
            "Private company limited by shares",
            "Starter Private company with limited liability",
            "Limited partnership", "Partnership limited by shares",
            "General partnership", "One-man company with limited liability",
            "Private company with limited liability with a social aim",
            "Private Founding",
            "Professional partnership in the form of a private company with "\
            "limited liability",
            "Private company"
        ]
        public_list = [
            "Company limited by shares",
            "Public company limited by shares",
            "Public company of another legal form",
            "Public utility Founding",
            "Public institution"
        ]
        cooperative_list = [
            "Co-operative society with limited liability",
            "Co-operative society with limited liability with a social aim",
            "Co-operative society",
            "Co-operative society with unlimited joint and several liability",
            "Co-operative society with limited liability by way of "\
            "participating interest",
            "Public co-operative society"
        ]
        nonprofit_list = [
            "Non-profit association", "Private non-profit association",
            "Association in charge of missions",
            "International non-profit association",
            "Company limited by shares with a social aim"
        ]
        other_list = [
            "Foreign company whose nature is not described",
            "Provincial government control",
            "Project Association",
            "Autonomous local authority with legal personality",
            "Economic joint venture with Belgian office",
            "European economic joint venture with Belgian office",
            "Public utility founding",
            "Economic joint venture",
            "Foreign company with a permanent office in Belgium"
        ]
        categories =  [private_list, public_list, cooperative_list,
    
                       nonprofit_list, other_list]
        labels = ["private", "public", "cooperative", "nonprofit", "other"]
    
        remap = {}
        for i, j in zip(categories, labels):
            for k in i:
                remap[k] = j
    
        legal_forms_transformed = self.df["Legal form"].replace(remap)
        legal_forms_transformed_dummies = pd.get_dummies(legal_forms_transformed)
    
        # Create dummies
        self.transf_df = self.transf_df.merge(legal_forms_transformed_dummies,
                                              how="left", left_index=True,
                                              right_index=True)

    def __add_peer_group(self):
        self.transf_df["peer_group"] = self.df["Peer Group Description"]


    def __add_size(self):
        self.transf_df["size"] = self.df["Category of the company"]


    def __add_leverage_variables(self):
        # Add total liabilities / tangible total assets
        total_liabilities_cols = [i for i in self.df.columns
                                     if "total liabilities th" in i.lower()]
        net_assets_cols = [i for i in self.df.columns
                           if "net current assets" in i.lower()]

        self.df["total_liabilities"] = \
            self.df[total_liabilities_cols].mean(axis=1) * 1000
        self.df["net_assets"] = \
            self.df[net_assets_cols].mean(axis=1)

        self.transf_df["total_liabilities_by_net_assets"] = \
            self.df["total_liabilities"] / self.df["net_assets"]


    def __add_liquidity_variables(self):
        # Add cash / total assets
        cash_cols = [i for i in self.df.columns if "cash" in i.lower()]
        total_assets_cols = [i for i in self.df.columns
                             if "total assets th" in i.lower()]
        self.df["cash"] = self.df[cash_cols].mean(axis=1)
        self.df["total_assets"] = self.df[total_assets_cols].mean(axis=1) * 1000
        self.transf_df["cash_by_total_assets"] = \
            self.df["cash"] / self.df["total_assets"]

        # Add cash / current liabilities
        current_liabilities_cols = [i for i in self.df.columns
                                    if "current liabilities th" in i.lower()]
        self.df["current_liabilities"] = \
            self.df[current_liabilities_cols].mean(axis=1) * 1000
        self.transf_df["cash_by_current_liabilities"] = \
            self.df["cash"] / self.df["current_liabilities"]

        # Add current assets / current liabilities
        current_assets_cols = [i for i in self.df.columns
                                    if "current assets th" in i.lower()]
        self.df["current_assets"] = \
            self.df[current_assets_cols].mean(axis=1) * 1000
        self.transf_df["current_assets_by_current_liabilities"] = \
            self.df["current_assets"] / self.df["current_liabilities"]


    def __add_profitability_variables(self):
        # Add earnings before interest
        # TODO: add the interest ot be able to calculate this variable

        # Add tax
        tax_cols = [i for i in self.df.columns
                    if "income taxes  th" in i.lower()]
        self.transf_df["tax"] = self.df[tax_cols].mean(axis=1) * 1000

        # Add EBITDA
        ebitda_cols = [i for i in self.df.columns
                       if "ebitda th" in i.lower()]
        self.transf_df["EBTIDA"] = self.df[ebitda_cols].mean(axis=1) * 1000

        # Add ROE (replaced by return on total assets)
        # TODO: add the P/L to be able to calculate this variable


    def __add_financing_variables(self):
        # Add financial expenses / total assets
        fin_exp_cols = [i for i in self.df.columns
                        if "financial charges" in i.lower()]
        self.df["financial_charges"] = self.df[fin_exp_cols].mean(axis=1) * 1000
        self.transf_df["financial_expenses_by_total_assets"] = \
            self.df["financial_charges"] / self.df["total_assets"] 

        # Add earnings before interest taxes
        EBIT_cols = [i for i in self.df.columns if "ebit th" in i.lower()]
        self.transf_df["EBIT"] = self.df[EBIT_cols].mean(axis=1) * 1000


    def __add_activity_variables(self):
        # Add working capital / total assets
        working_cap_cols = [i for i in self.df.columns
                        if "working capital th" in i.lower()]
        self.df["working_capital"] = \
            self.df[working_cap_cols].mean(axis=1) * 1000
        self.transf_df["working_capital_by_total_assets"] = \
            self.df["working_capital"] / self.df["total_assets"] 


    def __add_growth_variables(self):
        # TODO: add size classification
        self.transf_df["size_classification"] = \
            self.df["Category of the company"]
        # TODO: add number of employees as proxy for size
        pass


    def __add_other_variables(self):
        # Add income tax / total assets
        self.transf_df["income_tax_by_total_assets"] = \
            self.transf_df["tax"] / self.df["total_assets"]


    def df_to_csv(self):
        self.df.to_csv("data/temp_df.csv")


    def to_csv(self):
        self.transf_df.to_csv("data/bankruptcy_transformed.csv")


def main():
    dataset = BankruptcyDataset(path="data/Bel-first_Export_6.txt")
    dataset.create_transf_df()    
    dataset.df_to_csv()
    dataset.to_csv()


if __name__=="__main__":
    main()
