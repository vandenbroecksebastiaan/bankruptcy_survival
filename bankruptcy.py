import pandas as pd
import numpy as np


class BankruptcyDataset:
    def __init__(self, path):
        self.df = self.__read_df(path)


    def create_transf_df(self):
        """Transforsm the raw data from Bel-First to a dataset that is suitable
        for further analysis."""
        self.transf_df = pd.DataFrame(index=self.df.index.unique())
        self.transf_df.index = self.transf_df.index.rename("index")
        self.__add_status()
        self.__add_status_indicator()

        # Only keep companies with the following status
        self.transf_df = self.transf_df[self.transf_df["status"].isin(
            ["Liquidation", "Bankruptcy", "Merger by absorption"]
        )]
    
        self.__add_mean_pl()
        self.__add_mean_total_assets()
        self.__add_mean_n_employees()
        self.__add_legal_form()
        self.__add_peer_group()
        self.__add_size()


    def __read_df(self, path):
        df = pd.read_csv(path, delimiter="\t", encoding="UTF-16 LE")
        df = df[df["Mark"].notna()]
        df["Mark"] = df["Mark"].fillna(method="ffill")
        df = df.set_index("Mark")
        return df
    
    
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
            "Public utility founding"
        ]
        categories =  [private_list, public_list, cooperative_list,
    
                       nonprofit_list, other_list]
        labels = ["private", "public", "cooperative", "nonprofit", "other"]
    
        remap = {}
        for i, j in zip(categories, labels):
            for k in i:
                remap[k] = j
    
        self.df["Legal form"] = self.df["Legal form"].replace(remap)
    
        # Create dummies
        self.transf_df = self.transf_df.merge(pd.get_dummies(self.df["Legal form"]),
                                        how="left", left_index=True,
                                        right_index=True)

    def __add_peer_group(self):
        self.transf_df["peer_group"] = self.df["Peer Group Description"]


    def __add_size(self):
        self.transf_df["size"] = self.df["Category of the company"]


    def df_to_csv(self):
        self.df.to_csv("data/temp_df.csv")


    def to_csv(self):
        self.transf_df.to_csv("data/bankruptcy_transformed.csv")


def main():
    dataset = BankruptcyDataset(path="data/Bel-first_Export_4.txt")
    dataset.create_transf_df()    
    dataset.df_to_csv()
    dataset.to_csv()


if __name__=="__main__":
    main()
